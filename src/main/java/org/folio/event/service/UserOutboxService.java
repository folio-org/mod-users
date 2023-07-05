package org.folio.event.service;

import io.vertx.core.Context;
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
import org.folio.utils.HelperUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

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
          .map(logs.stream().map(OutboxEventLog::getEventId).collect(Collectors.toList()))
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
        if (isConsortiaTenant && isConsortiaFieldsUpdated) {
          return saveUserOutboxLog(conn, user, UserEvent.Action.EDIT, okapiHeaders);
        }
        return Future.succeededFuture();
      });
  }

  public Future<Boolean> saveUserOutboxLogForDeleteUsers(Conn conn, List<User> users, Map<String, String> okapiHeaders, Context context) {
    return userTenantService.isConsortiaTenant(conn, okapiHeaders)
      .compose(isConsortiaTenant -> {
        if (isConsortiaTenant) {
          return HelperUtils.executeWithSemaphores(users,
            user -> saveUserOutboxLog(conn, user, UserEvent.Action.DELETE, okapiHeaders), context)
            .map(res -> null);
        }
        return Future.succeededFuture();
      });
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
    String user = Json.encode(entity);
    return saveOutboxLog(conn, action.value(), OutboxEventLog.EntityType.USER, user, okapiHeaders)
      .onSuccess(reply -> logger.info("Outbox log has been saved for user id: {}", entity.getId()))
      .onFailure(e -> logger.warn("Could not save outbox audit log for user with id {}", entity.getId(), e));
  }

  private List<Future<Boolean>> getKafkaFutures(List<OutboxEventLog> logs, Map<String, String> okapiHeaders) {
    List<Future<Boolean>> futures = new ArrayList<>();
    for (OutboxEventLog log : logs) {
      if (OutboxEventLog.EntityType.USER == log.getEntityType()) {
        User user = Json.decodeValue(log.getPayload(), User.class);
        UserEvent.Action userAction = UserEvent.Action.fromValue(log.getAction());
        futures.add(producer.sendUserEvent(user, userAction, okapiHeaders));
      }
    }
    return futures;
  }

  private Future<Boolean> saveOutboxLog(Conn conn,
                                        String action,
                                        OutboxEventLog.EntityType entityType,
                                        String entity,
                                        Map<String, String> okapiHeaders) {
    String tenantId = TenantTool.tenantId(okapiHeaders);

    OutboxEventLog log = new OutboxEventLog()
      .withEventId(UUID.randomUUID().toString())
      .withAction(action)
      .withActionDate(new Date())
      .withEntityType(entityType)
      .withPayload(entity);

    return outboxRepository.saveEventLog(conn, log, tenantId);
  }

  private boolean isConsortiumUserFieldsUpdated(User updatedUser, User userFromStorage) {
    if (ObjectUtils.notEqual(userFromStorage.getUsername(), updatedUser.getUsername())) {
      logger.info("The username has been updated to {}", updatedUser.getUsername());
      return true;
    }

    Personal oldPersonal = userFromStorage.getPersonal();
    Personal newPersonal = updatedUser.getPersonal();

    if (oldPersonal == null && newPersonal == null) {
      logger.info("Personal fields have not been updated");
      return false;
    }

    if (oldPersonal == null || newPersonal == null) {
      logger.info("Personal fields have been updated");
      return true;
    }

    return ObjectUtils.notEqual(oldPersonal.getEmail(), newPersonal.getEmail())
      || ObjectUtils.notEqual(oldPersonal.getPhone(), newPersonal.getPhone())
      || ObjectUtils.notEqual(oldPersonal.getMobilePhone(), newPersonal.getMobilePhone());
  }
}
