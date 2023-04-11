package org.folio.moduserstest;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;
import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;

public abstract class AbstractRestTest {

  public static final String TENANT_NAME = "diku";

  protected static boolean LOAD_SAMPLE_DATA = false;
  protected static boolean LOAD_REFERENCE_DATA = false;
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  protected static VertxModule module;
  protected static OkapiUrl okapiUrl;
  protected static OkapiHeaders okapiHeaders;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, LOAD_REFERENCE_DATA, LOAD_SAMPLE_DATA))
      .onComplete(context.succeedingThenComplete());
  }

  @AfterAll
  public static void after(VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        kafkaCluster.stop();
        PostgresClient.stopPostgresTester();
        return Future.succeededFuture();
      })
      .onComplete(future -> context.completeNow())
      .onFailure(context::failNow);
  }
}
