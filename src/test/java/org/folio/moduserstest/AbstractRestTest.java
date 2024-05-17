package org.folio.moduserstest;

import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;

import java.lang.reflect.Field;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

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
import org.folio.event.KafkaConfigSingleton;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.s3.client.FolioS3Client;
import org.folio.s3.client.S3ClientFactory;
import org.folio.s3.client.S3ClientProperties;
import org.folio.support.VertxModule;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.junit.BeforeClass;
import org.junit.jupiter.api.AfterAll;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.KafkaContainer;
import org.testcontainers.containers.localstack.LocalStackContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

public abstract class AbstractRestTest {
  private static final Logger LOG = LoggerFactory.getLogger(AbstractRestTest.class);
  private static FolioS3Client s3Client;
  public static final String TENANT_NAME = "diku";
  public static final String KAFKA_ENV_VALUE = "test-env";
  public static final String KAFKA_IMAGE_NAME = "confluentinc/cp-kafka:7.3.1";

  protected static VertxModule module;
  protected static OkapiUrl okapiUrl;
  protected static OkapiHeaders okapiHeaders;
  protected static KafkaProducer<String, String> kafkaProducer;
  protected static Vertx vertx;

  private static final KafkaContainer kafkaContainer =
    new KafkaContainer(DockerImageName.parse(KAFKA_IMAGE_NAME));

  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context, boolean hasData) {
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);

    kafkaContainer.start();
    String kafkaHost = kafkaContainer.getHost();
    String kafkaPort = String.valueOf(kafkaContainer.getFirstMappedPort());
    updateKafkaConfigField("envId", KAFKA_ENV_VALUE);
    updateKafkaConfigField("kafkaHost", kafkaHost);
    updateKafkaConfigField("kafkaPort", kafkaPort);
    System.setProperty("kafka-host", kafkaHost);
    System.setProperty("kafka-port", kafkaPort);

    kafkaProducer = createKafkaProducer();

    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, hasData, hasData))
      .onComplete(context.succeedingThenComplete());
    setUpClass();
  }

  @Container
  private static final LocalStackContainer localStackContainer = new LocalStackContainer(
    DockerImageName.parse("localstack/localstack:0.11.3")
  )
    .withServices(LocalStackContainer.Service.S3);

  @BeforeClass
  public static void setUpClass() {
    localStackContainer.start();
    System.setProperty("AWS_URL", localStackContainer.getEndpoint().toString());
    System.setProperty("AWS_REGION", localStackContainer.getRegion());
    System.setProperty("AWS_ACCESS_KEY_ID", localStackContainer.getAccessKey());
    System.setProperty("AWS_SECRET_ACCESS_KEY", localStackContainer.getSecretKey());
    System.setProperty("AWS_BUCKET", TENANT_NAME);
    s3Client = S3ClientFactory.getS3Client(
      S3ClientProperties
        .builder()
        .endpoint(localStackContainer.getEndpoint().toString())
        .accessKey(localStackContainer.getAccessKey())
        .secretKey(localStackContainer.getSecretKey())
        .bucket("diku")
        .awsSdk(false)
        .region(localStackContainer.getRegion())
        .build()
    );
    s3Client.createBucketIfNotExists();
  }

  @AfterAll
  public static void after(VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        kafkaProducer.close();
        kafkaContainer.stop();
        PostgresClient.stopPostgresTester();
        return Future.succeededFuture();
      })
      .onComplete(context.succeedingThenComplete());
  }

  public List<String> checkKafkaEventSent(String tenant, String eventType) {
    Properties consumerProperties = new Properties();
    consumerProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    consumerProperties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    consumerProperties.put(ConsumerConfig.GROUP_ID_CONFIG, "test-group");
    consumerProperties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    ConsumerRecords<String, String> records;
    try (KafkaConsumer<String, String> kafkaConsumer = new KafkaConsumer<>(consumerProperties)) {
      kafkaConsumer.seekToBeginning(kafkaConsumer.assignment());

      kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
      records = kafkaConsumer.poll(Duration.ofMillis(3000));
    }
    return IteratorUtils.toList(records.iterator()).stream()
        .map(ConsumerRecord::value).toList();
  }

  private static KafkaProducer<String, String> createKafkaProducer() {
    Properties producerProperties = new Properties();
    producerProperties.setProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, kafkaContainer.getBootstrapServers());
    producerProperties.setProperty(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    producerProperties.setProperty(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class.getName());
    return new KafkaProducer<>(producerProperties);
  }

  @SneakyThrows
  public RecordMetadata sendEvent(String tenantId, String topic, String key, String value) {
    String topicName = formatToKafkaTopicName(tenantId, topic);
    ProducerRecord<String, String> producerRecord = new ProducerRecord<>(topicName, key, value);
    producerRecord.headers().add(OKAPI_TENANT_HEADER, TENANT_NAME.getBytes());
    return kafkaProducer.send(producerRecord).get();
  }

  private static String formatToKafkaTopicName(String tenant, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), tenant, eventType);
  }

  public static void updateKafkaConfigField(String fieldName, String newValue) {
    try {
      KafkaConfigSingleton instance = KafkaConfigSingleton.INSTANCE;
      Field kafkaConfigField = KafkaConfigSingleton.class.getDeclaredField("kafkaConfig");
      kafkaConfigField.setAccessible(true);

      KafkaConfig kafkaConfig = (KafkaConfig) kafkaConfigField.get(instance);
      Field envIdField = KafkaConfig.class.getDeclaredField(fieldName);
      envIdField.setAccessible(true);
      envIdField.set(kafkaConfig, newValue);

      kafkaConfigField.set(instance, kafkaConfig);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      LOG.error("Could not update kafka config", e);
    }
  }
}
