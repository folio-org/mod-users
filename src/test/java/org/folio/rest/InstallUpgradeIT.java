package org.folio.rest;

import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.is;

import java.nio.file.Path;
import java.util.UUID;

import io.restassured.RestAssured;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.folio.extensions.KafkaContainerExtension;
import org.folio.support.tags.IntegrationTest;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;
import org.testcontainers.kafka.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

@IntegrationTest
class InstallUpgradeIT {

  private static final Logger LOG = LoggerFactory.getLogger(InstallUpgradeIT.class);
  private static final boolean IS_LOG_ENABLED = false;
  private static final String KAFKA_NETWORK_ALIAS = UUID.randomUUID().toString();
  private static final Network NETWORK = Network.newNetwork();

  private static final KafkaContainer KAFKA =
    new KafkaContainer(DockerImageName.parse(KafkaContainerExtension.KAFKA_IMAGE_ID))
      .withNetwork(NETWORK)
      .withNetworkAliases(KAFKA_NETWORK_ALIAS)
      .withListener(KAFKA_NETWORK_ALIAS + ":9095");

  private static final GenericContainer<?> MOD_USERS =
    new GenericContainer<>(
      new ImageFromDockerfile("mod-users").withFileFromPath(".", Path.of(".")))
      .withNetwork(NETWORK)
      .withExposedPorts(8081, 5005)
      .withEnv("JAVA_OPTIONS", "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005")
      .withEnv("KAFKA_HOST", KAFKA_NETWORK_ALIAS)
      .withEnv("KAFKA_PORT", "9095");

  @BeforeAll
  static void beforeAll() {
    KAFKA.start();
    MOD_USERS.start();
    RestAssured.reset();
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = "http://" + MOD_USERS.getHost() + ":" + MOD_USERS.getFirstMappedPort();
    if (IS_LOG_ENABLED) {
      KAFKA.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
      MOD_USERS.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
    }
  }

  @AfterAll
  static void afterAll() {
    MOD_USERS.stop();
    KAFKA.stop();
    NETWORK.close();
  }

  @BeforeEach
  void beforeEach() {
    RestAssured.requestSpecification = null;
  }

  @Test
  void health() {
    when().
      get("/admin/health").
      then().
      statusCode(200).
      body(is("\"OK\""));
  }
}
