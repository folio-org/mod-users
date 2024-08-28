package org.folio.verticle.consumers;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.event.service.UserTenantService;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.kafka.KafkaHeaderUtils;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.utils.OkapiConnectionParams;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;

public class UserUpdateEventsHandler implements AsyncRecordHandler<String, String> {

  private static final Logger logger = LogManager.getLogger(UserUpdateEventsHandler.class);

  private final Vertx vertx;
  private final UserTenantService userTenantService;

  public UserUpdateEventsHandler(Vertx vertx) {
    this.vertx = vertx;
    this.userTenantService = new UserTenantService();
  }

  @Override
  public Future<String> handle(KafkaConsumerRecord<String, String> record) {
    List<KafkaHeader> kafkaHeaders = record.headers();
    OkapiConnectionParams okapiConnectionParams = new OkapiConnectionParams(KafkaHeaderUtils.kafkaHeadersToMap(kafkaHeaders), vertx);
    UserTenant event = new JsonObject(record.value()).mapTo(UserTenant.class);
    logger.info("Trying to handle user updates event: {} in tenant: {}", record.value(), okapiConnectionParams.getTenantId());
    return userTenantService.updateUserTenant(event, okapiConnectionParams.getTenantId(), okapiConnectionParams.getVertx())
      .map(event.getId())
      .onSuccess(x -> logger.info("User primary affiliation with event id: {} has been updated for user id: {} with tenant id: {}",
        event.getId(), event.getUserId(), event.getTenantId()))
      .onFailure(e -> logger.error("Failed to update user primary affiliation with event id: {} for user id: {} with tenant id: {}",
        event.getId(), event.getUserId(), event.getTenantId(), e));
  }
}
