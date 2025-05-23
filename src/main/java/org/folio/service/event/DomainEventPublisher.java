package org.folio.service.event;

import static org.apache.logging.log4j.LogManager.getLogger;

import java.util.Map;

import org.apache.logging.log4j.Logger;
import org.folio.event.KafkaConfigSingleton;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaProducerManager;
import org.folio.kafka.SimpleKafkaProducerManager;
import org.folio.kafka.services.KafkaEnvironmentProperties;
import org.folio.kafka.services.KafkaProducerRecordBuilder;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.kafka.client.producer.KafkaProducer;
import io.vertx.kafka.client.producer.KafkaProducerRecord;

public class DomainEventPublisher<K, T> {

  private static final Logger log = getLogger(DomainEventPublisher.class);

  private final String kafkaTopic;
  private final KafkaProducerManager producerManager;
  private final FailureHandler failureHandler;

  DomainEventPublisher(String kafkaTopic, KafkaProducerManager producerManager,
    FailureHandler failureHandler) {

    this.kafkaTopic = kafkaTopic;
    this.producerManager = producerManager;
    this.failureHandler = failureHandler;
  }

  public DomainEventPublisher(Context vertxContext, String kafkaTopic, FailureHandler failureHandler) {
    this(kafkaTopic, createProducerManager(vertxContext), failureHandler);
  }

  public Future<Void> publish(K key, DomainEvent<T> event, Map<String, String> okapiHeaders) {
    if (!KafkaConfigSingleton.INSTANCE.isKafkaEnabled()) {
      return Future.succeededFuture();
    }
    log.info("publish:: key = {}, eventId = {}, type = {}, topic = {}", key, event.getId(),
      event.getType(), kafkaTopic);

    KafkaProducerRecord<K, String> producerRecord =
      new KafkaProducerRecordBuilder<K, DomainEvent<T>>(TenantTool.tenantId(okapiHeaders))
        .key(key)
        .value(event)
        .topic(kafkaTopic)
        .propagateOkapiHeaders(okapiHeaders)
        .build();

    KafkaProducer<K, String> producer = getOrCreateProducer();
    log.info("publish:: Producer created, sending the record...");

    return producer.send(producerRecord)
      .onComplete(r -> {
        producer.end(ear -> producer.close());
        if (r.succeeded()) {
          log.info("publish:: Succeeded sending domain event with key = {}, eventId = {}, type = {}, topic = {}",
            key, event.getId(), event.getType(), kafkaTopic);
        } else {
          log.error("publish:: Unable to send domain event with key = {}, eventId = {}, type = {}, topic = {}",
            key, event.getId(), event.getType(), kafkaTopic, r.cause());
          failureHandler.handle(r.cause(), producerRecord);
        }
      })
      .mapEmpty();
  }

  private KafkaProducer<K, String> getOrCreateProducer() {
    return getOrCreateProducer("");
  }

  private KafkaProducer<K, String> getOrCreateProducer(String prefix) {
    return producerManager.createShared(prefix + kafkaTopic);
  }

  private static KafkaProducerManager createProducerManager(Context vertxContext) {
    var kafkaConfig = KafkaConfig.builder()
        .kafkaPort(KafkaEnvironmentProperties.port())
        .kafkaHost(KafkaEnvironmentProperties.host())
        .build();

    return new SimpleKafkaProducerManager(vertxContext.owner(), kafkaConfig);
  }
}
