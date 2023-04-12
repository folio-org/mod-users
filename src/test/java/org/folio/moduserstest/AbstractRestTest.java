package org.folio.moduserstest;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
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

public abstract class AbstractRestTest {

  public static final String KAFKA_ENV = "ENV";
  public static final String TENANT_NAME = "diku";
  public static final String KAFKA_HOST = "KAFKA_HOST";
  public static final String KAFKA_PORT = "KAFKA_PORT";
  public static final String KAFKA_ENV_VALUE = "test-env";
  public static final List<String> KAFKA_CONTAINER_PORTS = List.of("11541:2181", "11542:9092", "11543:9093");

  protected static boolean LOAD_SAMPLE_DATA = false;
  protected static boolean LOAD_REFERENCE_DATA = false;

  protected static VertxModule module;
  protected static OkapiUrl okapiUrl;
  protected static OkapiHeaders okapiHeaders;
  protected static KafkaConsumer<String, String> kafkaConsumer;
  private static final KafkaContainer kafkaContainer =
    new KafkaContainer(DockerImageName.parse("confluentinc/cp-kafka:7.3.1"));

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
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

    Properties properties = new Properties();
    properties.put("bootstrap.servers", kafkaContainer.getBootstrapServers());
    properties.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer");
    properties.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer");
    properties.put("group.id", "test-group");
    properties.put("auto.offset.reset", "earliest");
    kafkaConsumer = new KafkaConsumer<>(properties);
    kafkaConsumer.seekToBeginning(kafkaConsumer.assignment());

    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, LOAD_REFERENCE_DATA, LOAD_SAMPLE_DATA))
      .onComplete(context.succeedingThenComplete());
  }

  @AfterAll
  public static void after(VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        kafkaConsumer.close();
        kafkaContainer.stop();
        PostgresClient.stopPostgresTester();
        return Future.succeededFuture();
      })
      .onComplete(future -> context.completeNow())
      .onFailure(context::failNow);
  }

  public List<String> checkKafkaEventSent(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    ConsumerRecords<String, String> records = kafkaConsumer.poll(Duration.ofMillis(3000));
    return IteratorUtils.toList(records.iterator()).stream()
        .map(ConsumerRecord::value).collect(Collectors.toList());
  }

  public void commitAllMessagesInTopic(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    kafkaConsumer.poll(Duration.ofMillis(500));
  }

  private static String formatToKafkaTopicName(String tenant, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), tenant, eventType);
  }
}
