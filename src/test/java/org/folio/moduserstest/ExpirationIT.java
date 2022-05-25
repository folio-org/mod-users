package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;

import java.net.URI;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.ExpirationClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
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
  private static UsersClient usersClient;
  private static ExpirationClient expirationClient;

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    int port = NetworkUtils.nextFreePort();

    final var headers = new OkapiHeaders("http://localhost:" + port,
      "diku", "diku");

    usersClient = new UsersClient(new URI("http://localhost:" + port), headers);
    expirationClient = new ExpirationClient(
      new URI("http://localhost:" + port), headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .onComplete(context.succeeding(res -> module.enableModule(headers,
          false, false)
        .onComplete(context.succeedingThenComplete())));
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
  }

  @Test
  void canTriggerExpiration() {
    expirationClient.attemptToTriggerExpiration("diku")
      .statusCode(is(HTTP_NO_CONTENT));
  }

  @Test
  void cannotTriggerExpirationForUnknownTenant() {
    expirationClient.attemptToTriggerExpiration("made-up-tenant")
      .statusCode(is(HTTP_INTERNAL_ERROR));
  }
}
