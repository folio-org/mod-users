package org.folio.rest;

import io.restassured.RestAssured;
import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.is;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.KafkaContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;
import org.testcontainers.utility.DockerImageName;

import java.nio.file.Path;

/**
 * Check the shaded fat uber jar and Dockerfile.
 *
 * <p>Test /admin/health.
 *
 * <p>Test that logging works.
 *
 * <p>Test installation and migration with smoke test.
 *
 */
public class InstallUpgradeIT {

  private static final Logger LOG = LoggerFactory.getLogger(InstallUpgradeIT.class);
  private static final boolean IS_LOG_ENABLED = false;
  private static final Network NETWORK = Network.newNetwork();

  @ClassRule(order = 1)
  public static final KafkaContainer KAFKA =
    new KafkaContainer(DockerImageName.parse("confluentinc/cp-kafka:7.0.3"))
      .withNetwork(NETWORK)
      .withNetworkAliases("mykafka");

  @ClassRule(order = 2)
  public static final GenericContainer<?> MOD_USERS =
    new GenericContainer<>(
      new ImageFromDockerfile("mod-users").withFileFromPath(".", Path.of(".")))
      .withNetwork(NETWORK)
      .withExposedPorts(8081)
      .withEnv("KAFKA_HOST", "mykafka")
      .withEnv("KAFKA_PORT", "9092");

  @BeforeClass
  public static void beforeClass() {
    RestAssured.reset();
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = "http://" + MOD_USERS.getHost() + ":" + MOD_USERS.getFirstMappedPort();
    if (IS_LOG_ENABLED) {
      KAFKA.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
      MOD_USERS.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
    }
  }

  @Before
  public void beforeEach() {
    RestAssured.requestSpecification = null;
  }

  @Test
  public void health() {
    // request without X-Okapi-Tenant
    when().
      get("/admin/health").
      then().
      statusCode(200).
      body(is("\"OK\""));
  }

}
