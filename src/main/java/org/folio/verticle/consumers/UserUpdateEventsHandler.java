package org.folio.verticle.consumers;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.kafka.KafkaHeaderUtils;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.utils.OkapiConnectionParams;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;

public class UserUpdateEventsHandler implements AsyncRecordHandler<String, String> {

  private static final Logger logger = LogManager.getLogger(UserUpdateEventsHandler.class);

  private final Vertx vertx;

  public UserUpdateEventsHandler(Vertx vertx) {
    this.vertx = vertx;
  }

  @Override
  public Future<String> handle(KafkaConsumerRecord<String, String> record) {
    logger.info("Handling user update event: {}", record.value());
    List<KafkaHeader> kafkaHeaders = record.headers();
    OkapiConnectionParams okapiConnectionParams = new OkapiConnectionParams(KafkaHeaderUtils.kafkaHeadersToMap(kafkaHeaders), vertx);
    UserEvent event = new JsonObject(record.value()).mapTo(UserEvent.class);
    logger.info("Trying to handle user updates event: {} in tenant: {}", record.value(), okapiConnectionParams.getTenantId());
    return Future.succeededFuture(event.getId());
  }
}
