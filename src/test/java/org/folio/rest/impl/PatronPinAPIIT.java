
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;

import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.support.User;
import org.folio.support.http.PatronPinClient;
import org.folio.support.http.UsersClient;
import org.folio.test.util.DBTestUtil;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class PatronPinAPIIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static PatronPinClient patronPinClient;

  @BeforeAll
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    patronPinClient = new PatronPinClient(okapiUrl.asURI(), okapiHeaders);
  }

  @BeforeEach
  public void beforeEach(Vertx vertx, VertxTestContext context) {
    deleteAllPatronPins(vertx);
    usersClient.deleteAllUsers();

    // Context is unused, must be manually completed
    context.completeNow();
  }

  @ParameterizedTest
  @ValueSource(strings = {"1468", "ThisIsALonger1234PinWithSomeNumbers", "7778"})
  void canVerifyCorrectPatronPin(String pin) {
    final var user = usersClient.createUser("apple");

    patronPinClient.assignPatronPin(user.getId(), pin);

    enteredPinIsValid(user, pin);
  }

  @ParameterizedTest
  @CsvSource({"1468,1467", "ThisIsALonger1234PinWithSomeNumbers,1111"})
  void canVerifyIncorrectPatronPin(String actualPin, String attemptedPin) {
    final var user = usersClient.createUser("apple");

    patronPinClient.assignPatronPin(user.getId(), actualPin);

    enteredPinIsInvalid(user, attemptedPin);
  }

  @Test
  void canVerifyPatronPinForUserWithNoPinAssigned() {
    final var user = usersClient.createUser("apple");

    enteredPinIsInvalid(user, "1234");
  }

  @Test
  void canReassignPatronPin() {
    final var user = usersClient.createUser("apple");

    patronPinClient.assignPatronPin(user.getId(), "1234");

    patronPinClient.assignPatronPin(user.getId(), "4567");

    enteredPinIsValid(user, "4567");

    enteredPinIsInvalid(user, "1234");
  }

  @Test
  void canRemovePatronPin() {
    final var user = usersClient.createUser("apple");

    patronPinClient.assignPatronPin(user.getId(), "1234");

    patronPinClient.removePatronPin(user.getId());

    enteredPinIsInvalid(user, "1234");
  }

  private void enteredPinIsValid(User user, String pin) {
    patronPinClient.attemptToVerifyPatronPin(user.getId(), pin)
      .statusCode(is(200));
  }

  private void enteredPinIsInvalid(User user, String pin) {
    patronPinClient.attemptToVerifyPatronPin(user.getId(), pin)
      .statusCode(is(422));
  }

  private void deleteAllPatronPins(Vertx vertx) {
    DBTestUtil.deleteFromTable(vertx, "patronpin", TENANT_NAME);
  }

}
