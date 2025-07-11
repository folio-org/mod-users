package org.folio.extensions;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Properties;

import io.vertx.junit5.VertxTestContext;
import org.apache.kafka.clients.admin.AdminClient;
import org.apache.kafka.clients.admin.AdminClientConfig;
import org.apache.kafka.clients.admin.KafkaAdminClient;
import org.apache.kafka.clients.admin.NewTopic;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.kafka.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

import org.folio.event.KafkaConfigSingleton;
import org.folio.kafka.KafkaConfig;
import org.folio.support.TestConstants;

public class KafkaContainerExtension implements BeforeAllCallback, AfterAllCallback {

  private static final Logger LOG = LoggerFactory.getLogger(KafkaContainerExtension.class);

  public static final String KAFKA_IMAGE_ID = "apache/kafka-native:3.8.0";

  public static final KafkaContainer KAFKA_CONTAINER =
    new KafkaContainer(DockerImageName.parse(KAFKA_IMAGE_ID))
      .withStartupAttempts(3);

  @Override
  public void beforeAll(ExtensionContext context) {
    if (!KAFKA_CONTAINER.isRunning()) {
      KAFKA_CONTAINER.start();
    }

    System.setProperty("KAFKA_HOST", KAFKA_CONTAINER.getHost());
    System.setProperty("KAFKA_PORT", Integer.toString(KAFKA_CONTAINER.getFirstMappedPort()));
  }

  @Override
  public void afterAll(ExtensionContext context) {
    System.clearProperty("KAFKA_HOST");
    System.clearProperty("KAFKA_PORT");
  }

  public static String getBootstrapServers() {
    if (!KAFKA_CONTAINER.isRunning()) {
      LOG.warn("Kafka container is not running. Returning empty bootstrap servers.");
      return "";
    }
    return KAFKA_CONTAINER.getBootstrapServers();
  }

  public static void createTopics(List<String> topicNames) {
    var newTopics = topicNames.stream()
      .map(topicName -> new NewTopic(topicName, 1, (short) 1))
      .toList();

    try (var adminClient = getAdminClient()) {
      adminClient.createTopics(newTopics);
    } catch (Exception e) {
      throw new RuntimeException("Failed to create topics: " + topicNames, e);
    }
  }

  public static void deleteTopics(List<String> topicNames) {
    try (var adminClient = getAdminClient()) {
      adminClient.deleteTopics(topicNames);
    } catch (Exception e) {
      throw new RuntimeException("Failed to create topics: " + topicNames, e);
    }
  }

  public static AdminClient getAdminClient() {
    var props = new Properties();
    props.put(AdminClientConfig.BOOTSTRAP_SERVERS_CONFIG, getBootstrapServers());
    props.put(AdminClientConfig.REQUEST_TIMEOUT_MS_CONFIG, "5000");

    return KafkaAdminClient.create(props);
  }

  public static void enableKafka() {
    updateKafkaConfigField("envId", TestConstants.ENV);
    updateKafkaConfigField("kafkaHost", KAFKA_CONTAINER.getHost());
    updateKafkaConfigField("kafkaPort", Integer.toString(KAFKA_CONTAINER.getFirstMappedPort()));
  }

  public static void disableKafka(VertxTestContext context) {
    try {
      KafkaConfigSingleton instance = KafkaConfigSingleton.INSTANCE;
      Field enabledField = KafkaConfigSingleton.class.getDeclaredField("enabled");
      enabledField.setAccessible(true);
      enabledField.setBoolean(instance, false);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      context.failNow(e);
    }
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

      Field enabledField = KafkaConfigSingleton.class.getDeclaredField("enabled");
      enabledField.setAccessible(true);
      enabledField.setBoolean(instance, true);

      kafkaConfigField.set(instance, kafkaConfig);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      LOG.error("Could not update kafka config", e);
    }
  }
}
