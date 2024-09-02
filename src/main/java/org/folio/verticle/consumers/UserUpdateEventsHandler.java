package org.folio.verticle.consumers;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.service.event.DomainEventMapper.toEntityChangedEvent;
import static org.folio.service.event.DomainEventType.UPDATED;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.event.service.UserUpdateService;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.kafka.KafkaHeaderUtils;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.utils.OkapiConnectionParams;
import org.folio.service.event.DomainEvent;
import org.folio.service.event.EntityChangedData;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;

public class UserUpdateEventsHandler implements AsyncRecordHandler<String, String> {

  private static final Logger logger = LogManager.getLogger(UserUpdateEventsHandler.class);

  private final Vertx vertx;
  private final UserUpdateService userUpdateService;

  public UserUpdateEventsHandler(Vertx vertx) {
    this.vertx = vertx;
    this.userUpdateService = new UserUpdateService();
  }

  @Override
  public Future<String> handle(KafkaConsumerRecord<String, String> kafkaConsumerRecord) {
    final String eventKey = kafkaConsumerRecord.key();
    final String eventValue = kafkaConsumerRecord.value();
    logger.info("handle:: event received: key={}", eventKey);
    final DomainEvent<EntityChangedData<JsonObject>> event = toEntityChangedEvent(eventValue);
    if (event.getType() != UPDATED) {
      logger.warn("handle:: unsupported event type: {}", event.getType());
      return succeededFuture(eventKey);
    }
    logger.info("handle:: trying to update user with event id: {} with tenant: {}",
      event.getId(), event.getTenant());

    List<KafkaHeader> kafkaHeaders = kafkaConsumerRecord.headers();
    OkapiConnectionParams okapiConnectionParams = new OkapiConnectionParams(KafkaHeaderUtils.kafkaHeadersToMap(kafkaHeaders), vertx);
    User oldEntity = event.getData().getOldEntity().mapTo(User.class);
    User newEntity = event.getData().getNewEntity().mapTo(User.class);
    return userUpdateService.updateUser(oldEntity, newEntity,
        okapiConnectionParams.getTenantId(), okapiConnectionParams.getVertx(),
        okapiConnectionParams.getHeaders())
      .map(eventKey)
      .onSuccess(x -> logger.info("handle::user update event with id: {} has been " +
          "updated for user id: {} with tenant id: {}",
        event.getId(), oldEntity.getId(), event.getTenant()))
      .onFailure(e -> logger.error("handle:: failed update user with event id: {} for user id: {} " +
          "with tenant id: {}",
        event.getId(), oldEntity.getId(), event.getTenant()));
  }
}
