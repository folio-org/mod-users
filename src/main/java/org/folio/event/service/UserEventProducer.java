package org.folio.event.service;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.kafka.client.producer.KafkaHeader;
import io.vertx.kafka.client.producer.KafkaProducer;
import io.vertx.kafka.client.producer.KafkaProducerRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.event.KafkaConfigSingleton;
import org.folio.event.UserEventType;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaHeaderUtils;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.tools.utils.TenantTool;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class UserEventProducer {
  private static final Logger logger = LogManager.getLogger(UserEventProducer.class);

  private final KafkaConfig kafkaConfig;

  public UserEventProducer() {
    this(KafkaConfigSingleton.INSTANCE.getKafkaConfig());
  }

  public UserEventProducer(KafkaConfig kafkaConfig) {
    this.kafkaConfig = kafkaConfig;
  }

  public Future<Boolean> sendUserCreatedEvent(User user,
                                              UserEvent.Action eventAction,
                                              Map<String, String> okapiHeaders) {
    String tenantId = okapiHeaders.get("x-okapi-tenant");
    UserEvent event = getUserEvent(user, tenantId, eventAction);
    logger.info("Starting to send event with id: {} for User to Kafka for userId: {}", event.getId(), user.getId());
    String eventPayload = Json.encode(event);
    return sendToKafka(UserEventType.USER_CREATED, eventPayload, okapiHeaders, user.getId());
  }

  private UserEvent getUserEvent(User user, String tenantId, UserEvent.Action eventAction) {
    Metadata metadata = user.getMetadata();
    return new UserEvent()
      .withId(UUID.randomUUID().toString())
      .withAction(eventAction)
      .withEventDate(new Date())
      .withTenantId(tenantId)
      .withActionDate(metadata.getCreatedDate())
      .withPerformedBy(metadata.getUpdatedByUserId())
      .withUserDto(user.withPersonal(null).withMetadata(null));
  }

  private Future<Boolean> sendToKafka(UserEventType eventType,
                                      String eventPayload,
                                      Map<String, String> okapiHeaders,
                                      String key) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    List<KafkaHeader> kafkaHeaders = KafkaHeaderUtils.kafkaHeadersFromMap(okapiHeaders);
    String topicName = createTopicName(kafkaConfig.getEnvId(), tenantId, eventType.getTopicName());
    KafkaProducerRecord<String, String> producerRecord = createProducerRecord(topicName, key, eventPayload, kafkaHeaders);

    Promise<Boolean> promise = Promise.promise();

    KafkaProducer<String, String> producer = createProducer(eventType.getTopicName());
    producer.write(producerRecord, ar -> {
      producer.end(ear -> producer.close());
      if (ar.succeeded()) {
        logger.info("Event with type '{}' for user with id: '{}' was sent to kafka topic '{}'", eventType, key, topicName);
        promise.complete(true);
      } else {
        Throwable cause = ar.cause();
        logger.error("Producer write error for event '{}' for user with id: '{}' for kafka topic '{}'",  eventType, key, topicName, cause);
        promise.fail(cause);
      }
    });

    return promise.future();
  }

  private KafkaProducer<String, String> createProducer(String eventType) {
    String producerName = eventType + "_Producer";
    return KafkaProducer.createShared(Vertx.currentContext().owner(), producerName, kafkaConfig.getProducerProps());
  }

  private KafkaProducerRecord<String, String> createProducerRecord(String topicName, String key, String eventPayload, List<KafkaHeader> kafkaHeaders) {
    return KafkaProducerRecord.create(topicName, key, eventPayload)
      .addHeaders(kafkaHeaders);
  }

  private String createTopicName(String envId, String tenantId, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(envId, KafkaTopicNameHelper.getDefaultNameSpace(),
      tenantId, eventType);
  }

}
