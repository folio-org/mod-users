package org.folio.support.kafka;

import static org.apache.kafka.clients.producer.ProducerConfig.BOOTSTRAP_SERVERS_CONFIG;
import static org.apache.kafka.clients.producer.ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG;
import static org.apache.kafka.clients.producer.ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG;
import static org.folio.extensions.KafkaContainerExtension.getTopicName;
import static org.folio.extensions.KafkaContainerExtension.getBootstrapServers;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.support.TestConstants.TENANT_NAME;

import java.time.Duration;
import java.util.Properties;

import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.serialization.StringSerializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import lombok.SneakyThrows;

public class TestKafkaProducer {

  private static final Logger LOG = LoggerFactory.getLogger(TestKafkaProducer.class);

  private final KafkaProducer<String, String> kafkaProducer;

  public TestKafkaProducer() {
    try {
      this.kafkaProducer = new KafkaProducer<>(getKafkaProducerProperties());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  public void close() {
    kafkaProducer.close();
  }

  @SneakyThrows
  @SuppressWarnings("UnusedReturnValue")
  public RecordMetadata sendEvent(String tenantId, String topic, String key, String value) {
    String topicName = getTopicName(tenantId, topic);
    ProducerRecord<String, String> producerRecord = new ProducerRecord<>(topicName, key, value);
    producerRecord.headers().add(OKAPI_TENANT_HEADER, TENANT_NAME.getBytes());
    LOG.info("Sending message to topic: topic={}, value={}", topicName, value);
    return kafkaProducer.send(producerRecord).get();
  }

  private static Properties getKafkaProducerProperties() {
    Properties config = new Properties();
    config.setProperty(BOOTSTRAP_SERVERS_CONFIG, getBootstrapServers());
    config.setProperty(KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    config.setProperty(VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    return config;
  }
}
