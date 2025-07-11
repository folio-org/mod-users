package org.folio.moduserstest;

import static io.vertx.core.Future.succeededFuture;
import static java.time.Duration.ofMillis;
import static java.time.Duration.ofMinutes;
import static org.folio.extensions.KafkaContainerExtension.createTopics;
import static org.folio.extensions.KafkaContainerExtension.deleteTopics;
import static org.folio.extensions.KafkaContainerExtension.getBootstrapServers;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.support.TestConstants.ENV;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.folio.test.util.TestUtil.readFile;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
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
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.shaded.org.awaitility.Awaitility;
import org.testcontainers.shaded.org.awaitility.core.ThrowingRunnable;

import org.folio.event.ConsortiumEventType;
import org.folio.extensions.KafkaContainerExtension;
import org.folio.extensions.LocalStackContainerExtension;
import org.folio.extensions.PostgresContainerExtension;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import lombok.SneakyThrows;

@ExtendWith({
  VertxExtension.class,
  KafkaContainerExtension.class,
  LocalStackContainerExtension.class,
  PostgresContainerExtension.class,
})
public abstract class AbstractRestTest {

  private static final Logger LOG = LoggerFactory.getLogger(AbstractRestTest.class);

  protected static OkapiUrl okapiUrl;
  protected static VertxModule module;
  protected static OkapiHeaders okapiHeaders;
  protected static KafkaProducer<String, String> kafkaProducer;

  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context, boolean hasData) {
    System.setProperty("ENV", ENV);
    KafkaContainerExtension.enableKafka();
    createTopics(getConsortiumTopicNames());
    final var port = NetworkUtils.nextFreePort();
    final var token = new FakeTokenGenerator().generateToken();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);
    module = new VertxModule(vertx);
    kafkaProducer = createKafkaProducer();

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, hasData, hasData))
      .onComplete(context.succeedingThenComplete());
  }

  private static List<String> getConsortiumTopicNames() {
    return Arrays.stream(ConsortiumEventType.values())
      .map(ConsortiumEventType::getTopicName)
      .map(eventType -> formatToKafkaTopicName(TENANT_NAME, eventType))
      .toList();
  }

  @AfterAll
  static void afterAll(Vertx vertx, VertxTestContext context) {
    deleteTopics(getConsortiumTopicNames());
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        kafkaProducer.close();
        return succeededFuture();
      })
      .onComplete(context.succeedingThenComplete())
      .onComplete(e -> vertx.close());
  }

  public List<String> checkKafkaEventSent(String tenant, String eventType) {
    var consumerProperties = getConsumerProperies();
    ConsumerRecords<String, String> records;
    try (KafkaConsumer<String, String> kafkaConsumer = new KafkaConsumer<>(consumerProperties)) {
      kafkaConsumer.seekToBeginning(kafkaConsumer.assignment());

      kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
      records = kafkaConsumer.poll(ofMillis(250));
    }
    return IteratorUtils.toList(records.iterator()).stream()
      .map(ConsumerRecord::value).toList();
  }

  private static @NotNull Properties getConsumerProperies() {
    Properties consumerProperties = new Properties();
    consumerProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, getBootstrapServers());
    consumerProperties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.GROUP_ID_CONFIG, "test-group");
    consumerProperties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    return consumerProperties;
  }

  private static KafkaProducer<String, String> createKafkaProducer() {
    Properties producerProperties = new Properties();
    producerProperties.setProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, getBootstrapServers());
    producerProperties.setProperty(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    producerProperties.setProperty(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    return new KafkaProducer<>(producerProperties);
  }

  @SneakyThrows
  public RecordMetadata sendEvent(String tenantId, String topic, String key, String value) {
    String topicName = formatToKafkaTopicName(tenantId, topic);
    ProducerRecord<String, String> producerRecord = new ProducerRecord<>(topicName, key, value);
    producerRecord.headers().add(OKAPI_TENANT_HEADER, TENANT_NAME.getBytes());
    LOG.info("Sending message to topic: topic={}, value={}", topicName, value);
    return kafkaProducer.send(producerRecord).get();
  }

  private static String formatToKafkaTopicName(String tenant, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(ENV, getDefaultNameSpace(), tenant, eventType);
  }

  protected static void awaitUntilAsserted(ThrowingRunnable runnable) {
    awaitUntilAsserted(runnable, ofMinutes(1), ofMillis(500));
  }

  protected static void awaitUntilAsserted(ThrowingRunnable runnable,
    Duration atMostDuration, Duration pollIntervalDuration) {
    Awaitility.await()
      .atMost(atMostDuration)
      .pollInterval(pollIntervalDuration)
      .untilAsserted(runnable);
  }

  protected static <T> T readObjectFromFile(String pathToJson, Class<T> clazz) {
    var jsonString = readExistedFile(pathToJson);
    assertThat(jsonString, notNullValue());
    return Json.decodeValue(jsonString, clazz);
  }

  protected static String readExistedFile(String pathToJson) {
    try {
      return readFile(pathToJson);
    } catch (IOException | URISyntaxException e) {
      Assertions.fail(e.getMessage());
      return null;
    }
  }
}
