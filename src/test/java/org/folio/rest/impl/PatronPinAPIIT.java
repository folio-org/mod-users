
package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_OK;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.support.TestConstants.TENANT_NAME;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.support.User;
import org.folio.support.http.PatronPinClient;
import org.folio.support.http.UsersClient;
import org.folio.support.tags.IntegrationTest;
import org.folio.test.util.DBTestUtil;

@IntegrationTest
class PatronPinAPIIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static PatronPinClient patronPinClient;

  @BeforeAll
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    patronPinClient = new PatronPinClient(okapiUrl.asURI(), okapiHeaders);
  }

  @BeforeEach
  void beforeEach(Vertx vertx, VertxTestContext context) {
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
      .statusCode(SC_OK);
  }

  private void enteredPinIsInvalid(User user, String pin) {
    patronPinClient.attemptToVerifyPatronPin(user.getId(), pin)
      .statusCode(SC_UNPROCESSABLE_ENTITY);
  }

  private void deleteAllPatronPins(Vertx vertx) {
    DBTestUtil.deleteFromTable(vertx, "patronpin", TENANT_NAME);
  }

}
