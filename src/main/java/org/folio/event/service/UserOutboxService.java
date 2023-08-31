package org.folio.event.service;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.jaxrs.model.OutboxEventLog;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.repository.UserEventsLogRepository;
import org.folio.repository.InternalLockRepository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class UserOutboxService {

  private static final Logger logger = LogManager.getLogger(UserOutboxService.class);
  private static final String OUTBOX_LOCK_NAME = "user_outbox";

  private final UserEventProducer producer;
  private final InternalLockRepository lockRepository;
  private final UserEventsLogRepository outboxRepository;
  private final UserTenantService userTenantService;

  public UserOutboxService() {
    producer = new UserEventProducer();
    lockRepository = new InternalLockRepository();
    outboxRepository = new UserEventsLogRepository();
    userTenantService = new UserTenantService();
  }

  /**
   * Reads outbox event logs from DB and send them to Kafka
   * and delete from outbox table in the single transaction.
   *
   * @param okapiHeaders the okapi headers
   * @return future with integer how many records have been processed
   */
  public Future<Integer> processOutboxEventLogs(Vertx vertx, Map<String, String> okapiHeaders) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    PostgresClient pgClient = PostgresClient.getInstance(vertx, tenantId);
    return pgClient.withTrans(conn -> lockRepository.selectWithLocking(conn, OUTBOX_LOCK_NAME, tenantId)
      .compose(retrievedCount -> outboxRepository.fetchEventLogs(conn, tenantId))
      .compose(logs -> {
        if (CollectionUtils.isEmpty(logs)) {
          return Future.succeededFuture(0);
        }

        logger.info("Fetched {} event logs from outbox table, going to send them to kafka", logs.size());
        List<Future<Boolean>> futures = getKafkaFutures(logs, okapiHeaders);
        return GenericCompositeFuture.join(futures)
          .map(logs.stream().map(OutboxEventLog::getEventId).toList())
          .compose(eventIds -> {
            if (CollectionUtils.isNotEmpty(eventIds)) {
              return outboxRepository.deleteBatch(conn, eventIds, tenantId)
                .onSuccess(rowsCount -> logger.info("{} logs have been deleted from outbox table", rowsCount))
                .onFailure(ex -> logger.error("Logs deletion failed", ex));
            }
            return Future.succeededFuture(0);
          });
      })
    );
  }

  public Future<Boolean> saveUserOutboxLogForCreateOrDeleteUser(Conn conn, User user, UserEvent.Action action, Map<String, String> okapiHeaders) {
      return userTenantService.isConsortiaTenant(conn, okapiHeaders)
        .compose(isConsortiaTenant -> {
          if (isConsortiaTenant) {
            return saveUserOutboxLog(conn, user, action, okapiHeaders);
          }
          return Future.succeededFuture();
        });
  }

  public Future<Boolean> saveUserOutboxLogForUpdateUser(Conn conn, User user, User userFromStorage, Map<String, String> okapiHeaders) {
    return userTenantService.isConsortiaTenant(conn, okapiHeaders)
      .compose(isConsortiaTenant -> {
        boolean isConsortiaFieldsUpdated = isConsortiumUserFieldsUpdated(user, userFromStorage);
        boolean isPersonalDataChanged = isPersonalDataChanged(user, userFromStorage);
        if (isConsortiaTenant && isConsortiaFieldsUpdated) {
          return saveUserOutboxLog(conn, user, isPersonalDataChanged, UserEvent.Action.EDIT, okapiHeaders);
        }
        return Future.succeededFuture();
      });
  }

  public Future<Boolean> saveUserOutboxLogForDeleteUsers(Conn conn, List<User> users, Map<String, String> okapiHeaders) {
    return userTenantService.isConsortiaTenant(conn, okapiHeaders)
      .compose(isConsortiaTenant -> {
        if (isConsortiaTenant) {
          List<Future<Boolean>> resultFuture = new ArrayList<>();
          Future<Boolean> lineFuture = Future.succeededFuture();
          for (User user : users) {
            lineFuture = lineFuture.compose(v -> saveUserOutboxLog(conn, user, UserEvent.Action.DELETE, okapiHeaders));
            resultFuture.add(lineFuture);
          }
          return Future.succeededFuture(resultFuture.size() == users.size());
        }
        return Future.succeededFuture(false);
      });
  }

  /**
   * Saves user outbox log.
   *
   * @param conn connection in transaction
   * @param entity the user
   * @param isPersonalDataChanged was the personal data changed
   * @param action the event action
   * @param okapiHeaders okapi headers
   * @return future with saved outbox log in the same transaction
   */
  public Future<Boolean> saveUserOutboxLog(Conn conn, User entity, boolean isPersonalDataChanged, UserEvent.Action action, Map<String, String> okapiHeaders) {
    String user = Json.encode(entity);
    return saveOutboxLog(conn, action.value(), OutboxEventLog.EntityType.USER, user, isPersonalDataChanged, okapiHeaders)
      .onSuccess(reply -> logger.info("Outbox log has been saved for user id: {}", entity.getId()))
      .onFailure(e -> logger.warn("Could not save outbox audit log for user with id {}", entity.getId(), e));
  }

  /**
   * Saves user outbox log.
   *
   * @param conn connection in transaction
   * @param entity the user
   * @param action the event action
   * @param okapiHeaders okapi headers
   * @return future with saved outbox log in the same transaction
   */
  public Future<Boolean> saveUserOutboxLog(Conn conn, User entity, UserEvent.Action action, Map<String, String> okapiHeaders) {
    return saveUserOutboxLog(conn, entity, false, action, okapiHeaders);
  }

  private List<Future<Boolean>> getKafkaFutures(List<OutboxEventLog> logs, Map<String, String> okapiHeaders) {
    List<Future<Boolean>> futures = new ArrayList<>();
    for (OutboxEventLog log : logs) {
      if (OutboxEventLog.EntityType.USER == log.getEntityType()) {
        User user = Json.decodeValue(log.getPayload(), User.class);
        boolean isPersonalDataChanged = log.getIsPersonalDataChanged() != null && log.getIsPersonalDataChanged();
        UserEvent.Action userAction = UserEvent.Action.fromValue(log.getAction());
        futures.add(producer.sendUserEvent(user, isPersonalDataChanged, userAction, okapiHeaders));
      }
    }
    return futures;
  }

  private Future<Boolean> saveOutboxLog(Conn conn,
                                        String action,
                                        OutboxEventLog.EntityType entityType,
                                        String entity,
                                        boolean isPersonalDataChanged,
                                        Map<String, String> okapiHeaders) {
    String tenantId = TenantTool.tenantId(okapiHeaders);

    OutboxEventLog log = new OutboxEventLog()
      .withEventId(UUID.randomUUID().toString())
      .withAction(action)
      .withActionDate(new Date())
      .withEntityType(entityType)
      .withPayload(entity)
      .withIsPersonalDataChanged(isPersonalDataChanged);

    return outboxRepository.saveEventLog(conn, log, tenantId);
  }

  private boolean isConsortiumUserFieldsUpdated(User updatedUser, User userFromStorage) {
    if (ObjectUtils.notEqual(userFromStorage.getUsername(), updatedUser.getUsername())
        || ObjectUtils.notEqual(userFromStorage.getBarcode(), updatedUser.getBarcode())
        || ObjectUtils.notEqual(userFromStorage.getExternalSystemId(), updatedUser.getExternalSystemId())) {
      logger.info("The user with id: {} has been updated", updatedUser.getId());
      return true;
    }

    Personal oldPersonal = userFromStorage.getPersonal();
    Personal newPersonal = updatedUser.getPersonal();

    if (oldPersonal == null && newPersonal == null) {
      logger.info("Personal fields have not been updated for user with id: {}", updatedUser.getId());
      return false;
    }

    if (oldPersonal == null || newPersonal == null) {
      logger.info("Personal fields have been updated for user with id: {}", updatedUser.getId());
      return true;
    }

    return ObjectUtils.notEqual(oldPersonal.getEmail(), newPersonal.getEmail())
      || ObjectUtils.notEqual(oldPersonal.getPhone(), newPersonal.getPhone())
      || ObjectUtils.notEqual(oldPersonal.getMobilePhone(), newPersonal.getMobilePhone());
  }

  private boolean isPersonalDataChanged(User updatedUser, User userFromStorage) {
    Personal oldPersonal = userFromStorage.getPersonal();
    Personal newPersonal = updatedUser.getPersonal();

    if (isPersonalNotNull(oldPersonal, newPersonal) && isPersonalNameChanged(oldPersonal, newPersonal)) {
      logger.info("Personal name have been updated for user with id: {}", updatedUser.getId());
      return true;
    }
    return false;
  }

  private boolean isPersonalNameChanged(Personal oldPersonal, Personal newPersonal) {
    return ObjectUtils.notEqual(oldPersonal.getFirstName(), newPersonal.getFirstName())
      || ObjectUtils.notEqual(oldPersonal.getLastName(), newPersonal.getLastName());
  }

  private boolean isPersonalNotNull(Personal oldPersonal, Personal newPersonal) {
    return oldPersonal != null && newPersonal != null;
  }
}
