package org.folio.event.service;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.kafka.client.producer.KafkaProducer;
import io.vertx.kafka.client.producer.KafkaProducerRecord;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.event.KafkaConfigSingleton;
import org.folio.event.UserEventType;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.services.KafkaProducerRecordBuilder;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UsersService;

import java.util.Date;
import java.util.Map;
import java.util.Objects;
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

  public Future<Boolean> sendUserEvent(User user,
                                       boolean isPersonalDataChanged,
                                       UserEvent.Action eventAction,
                                       Map<String, String> okapiHeaders) {
    String tenantId = okapiHeaders.get("x-okapi-tenant");
    UserEvent event = getUserEvent(user, tenantId, isPersonalDataChanged, eventAction);

    return switch (eventAction) {
      case CREATE -> {
        logger.info("Starting to send user created event with id: {} for User to Kafka for userId: {}", event.getId(), user.getId());
        yield sendToKafka(UserEventType.USER_CREATED, event, okapiHeaders, user.getId());
      }
      case DELETE -> {
        logger.info("Starting to send user deleted event with id: {} for User to Kafka for userId: {}", event.getId(), user.getId());
        yield sendToKafka(UserEventType.USER_DELETED, event, okapiHeaders, user.getId());
      }
      case EDIT -> {
        logger.info("Starting to send user edit event with id: {} for User to Kafka for userId: {}", event.getId(), user.getId());
        yield sendToKafka(UserEventType.USER_UPDATED, event, okapiHeaders, user.getId());
      }
    };
  }

  private UserEvent getUserEvent(User user, String tenantId, boolean isPersonalDataChanged, UserEvent.Action eventAction) {
    Metadata metadata = user.getMetadata();
    UserEvent event = new UserEvent();
    event.setId(UUID.randomUUID().toString());
    event.setAction(eventAction);
    event.setEventDate(new Date());
    event.setTenantId(tenantId);
    event.setIsPersonalDataChanged(isPersonalDataChanged);
    if (Objects.nonNull(metadata)) {
      event.setActionDate(metadata.getCreatedDate());
      event.setPerformedBy(metadata.getUpdatedByUserId());
    }
    event.setUser(UsersService.getConsortiumUserDto(user));

    return event;
  }

  private Future<Boolean> sendToKafka(UserEventType eventType,
                                      UserEvent userEvent,
                                      Map<String, String> okapiHeaders,
                                      String key) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    String topicName = createTopicName(kafkaConfig.getEnvId(), tenantId, eventType.getTopicName());
    KafkaProducerRecord<String, String> producerRecord = createProducerRecord(tenantId, topicName, key, userEvent, okapiHeaders);

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

  private KafkaProducerRecord<String, String> createProducerRecord(String tenantId, String topicName, String key, UserEvent userEvent, Map<String, String> okapiHeaders) {
    return new KafkaProducerRecordBuilder<String, Object>(tenantId)
      .key(key)
      .value(userEvent)
      .topic(topicName)
      .propagateOkapiHeaders(okapiHeaders)
      .build();
  }

  private String createTopicName(String envId, String tenantId, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(envId, KafkaTopicNameHelper.getDefaultNameSpace(),
      tenantId, eventType);
  }

}
