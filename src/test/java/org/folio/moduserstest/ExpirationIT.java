package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.util.concurrent.TimeUnit.SECONDS;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.time.ZonedDateTime;

import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.http.ExpirationClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class ExpirationIT {
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private final static String TENANT = "expirationit";
  private static UsersClient usersClient;
  private static ExpirationClient expirationClient;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    int port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, TENANT, "token");

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    usersClient = new UsersClient(okapiUrl, headers);
    expirationClient = new ExpirationClient(okapiUrl, headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers))
      .onComplete(context.succeedingThenComplete());
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
  }

  @Test
  void expiredUsersAreDisabled() {
    final var firstExpiredUser = usersClient.createUser(User.builder()
      .username("first-user")
      .active(true)
      .expirationDate(ZonedDateTime.now().minusHours(3))
      .build());

    final var secondExpiredUser = usersClient.createUser(User.builder()
      .username("second-user")
      .active(true)
      .expirationDate(ZonedDateTime.now().minusDays(15))
      .build());

    expirationClient.attemptToTriggerExpiration(TENANT)
      .statusCode(is(HTTP_NO_CONTENT));

    final var firstFetchedUser = usersClient.getUser(firstExpiredUser.getId());

    assertThat(firstFetchedUser.getActive(), is(false));

    final var secondFetchedUser = usersClient.getUser(secondExpiredUser.getId());

    assertThat(secondFetchedUser.getActive(), is(false));
  }

  @Test
  void unexpiredUsersAreNotDisabled() {
    final var unexpiredUser = usersClient.createUser(User.builder()
      .username("some-user")
      .active(true)
      .expirationDate(ZonedDateTime.now().plusHours(3))
      .build());

    expirationClient.attemptToTriggerExpiration(TENANT)
      .statusCode(is(HTTP_NO_CONTENT));

    final var fetchedUser = usersClient.getUser(unexpiredUser.getId());

    assertThat(fetchedUser.getActive(), is(true));
  }

  @Test
  void cannotTriggerExpirationForUnknownTenant() {
    expirationClient.attemptToTriggerExpiration("made-up-tenant")
      .statusCode(is(HTTP_INTERNAL_ERROR));
  }

  @AfterAll
  public static void after() {
    kafkaCluster.stop();
  }
}
