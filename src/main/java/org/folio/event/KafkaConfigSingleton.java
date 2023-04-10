package org.folio.event;

import org.folio.kafka.KafkaConfig;

import java.util.Optional;

public enum KafkaConfigSingleton {
  INSTANCE;

  private final KafkaConfig kafkaConfig;

  KafkaConfigSingleton() {
    String envId = getPropertyValue("ENV", "folio");
    String kafkaPort = getPropertyValue("KAFKA_PORT", "9092");
    String kafkaHost = getPropertyValue("KAFKA_HOST", "kafka");
    String okapiUrl = getPropertyValue("OKAPI_URL", "http://okapi:9130");
    int replicationFactor = Integer.parseInt(getPropertyValue("REPLICATION_FACTOR", "1"));
    int maxRequestSize = Integer.parseInt(getPropertyValue("MAX_REQUEST_SIZE", "1048576"));

    kafkaConfig = KafkaConfig.builder()
      .envId(envId)
      .kafkaHost(kafkaHost)
      .kafkaPort(kafkaPort)
      .okapiUrl(okapiUrl)
      .replicationFactor(replicationFactor)
      .maxRequestSize(maxRequestSize)
      .build();
  }

  public KafkaConfig getKafkaConfig() {
    return kafkaConfig;
  }

  private static String getPropertyValue(String propertyName, String defaultValue) {
    return Optional.ofNullable(System.getProperty(propertyName))
      .or(() -> Optional.ofNullable(System.getenv(propertyName)))
      .orElse(defaultValue);
  }

}
