package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.time.ZonedDateTime;

import org.folio.support.User;
import org.folio.support.http.TimerInterfaceClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class ExpirationIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static TimerInterfaceClient timerInterfaceClient;

  @BeforeAll
  public static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    timerInterfaceClient = new TimerInterfaceClient(okapiUrl, okapiHeaders);
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

    timerInterfaceClient.attemptToTriggerExpiration(TENANT_NAME)
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

    timerInterfaceClient.attemptToTriggerExpiration(TENANT_NAME)
      .statusCode(is(HTTP_NO_CONTENT));

    final var fetchedUser = usersClient.getUser(unexpiredUser.getId());
    assertThat(fetchedUser.getActive(), is(true));
  }

  @Test
  void cannotTriggerExpirationForUnknownTenant() {
    timerInterfaceClient.attemptToTriggerExpiration("made-up-tenant")
      .statusCode(is(HTTP_INTERNAL_ERROR));
  }
}
