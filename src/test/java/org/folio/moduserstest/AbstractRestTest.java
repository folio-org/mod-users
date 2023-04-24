package org.folio.moduserstest;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.testcontainers.containers.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;

public abstract class AbstractRestTest {

  public static final String KAFKA_ENV = "ENV";
  public static final String TENANT_NAME = "diku";
  public static final String KAFKA_HOST = "KAFKA_HOST";
  public static final String KAFKA_PORT = "KAFKA_PORT";
  public static final String KAFKA_ENV_VALUE = "test-env";
  public static final List<String> KAFKA_CONTAINER_PORTS = List.of("11541:2181", "11542:9092", "11543:9093");

  protected static VertxModule module;
  protected static OkapiUrl okapiUrl;
  protected static OkapiHeaders okapiHeaders;
  protected static KafkaConsumer<String, String> kafkaConsumer;
  protected static KafkaProducer<String, String> kafkaProducer;
  private static final KafkaContainer kafkaContainer =
    new KafkaContainer(DockerImageName.parse("confluentinc/cp-kafka:7.3.1"));

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context, boolean hasData) {
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);

    kafkaContainer.setPortBindings(KAFKA_CONTAINER_PORTS);
    kafkaContainer.start();
    System.setProperty(KAFKA_HOST, kafkaContainer.getHost());
    System.setProperty(KAFKA_PORT, String.valueOf(kafkaContainer.getFirstMappedPort()));
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    Properties consumerProperties = new Properties();
    consumerProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    consumerProperties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.GROUP_ID_CONFIG, "test-group");
    consumerProperties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    kafkaConsumer = new KafkaConsumer<>(consumerProperties);
    kafkaConsumer.seekToBeginning(kafkaConsumer.assignment());

    Properties producerProperties = new Properties();
    producerProperties.setProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    producerProperties.setProperty(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    producerProperties.setProperty(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    kafkaProducer = new KafkaProducer<>(producerProperties);

    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, hasData, hasData))
      .onComplete(context.succeedingThenComplete());
  }

  @AfterAll
  public static void after(VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        kafkaProducer.close();
        kafkaConsumer.close();
        kafkaContainer.stop();
        PostgresClient.stopPostgresTester();
        return Future.succeededFuture();
      })
      .onComplete(context.succeedingThenComplete());
  }

  public List<String> checkKafkaEventSent(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    ConsumerRecords<String, String> records = kafkaConsumer.poll(Duration.ofMillis(3000));
    return IteratorUtils.toList(records.iterator()).stream()
        .map(ConsumerRecord::value).collect(Collectors.toList());
  }

  @SneakyThrows
  public RecordMetadata sendEvent(String tenantId, String topic, String key, String value) {
    String topicName = formatToKafkaTopicName(tenantId, topic);
    ProducerRecord<String, String> record = new ProducerRecord<>(topicName, key, value);
    record.headers().add(OKAPI_TENANT_HEADER, TENANT_NAME.getBytes());
    return kafkaProducer.send(record).get();
  }

  public void commitAllMessagesInTopic(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    kafkaConsumer.poll(Duration.ofMillis(1000));
  }

  private static String formatToKafkaTopicName(String tenant, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), tenant, eventType);
  }
}
