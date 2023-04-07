
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.hamcrest.CoreMatchers.is;

import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.PatronPinClient;
import org.folio.support.http.UsersClient;
import org.folio.test.util.DBTestUtil;
import org.junit.jupiter.api.AfterAll;
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
import lombok.SneakyThrows;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class PatronPinAPIIT {
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private static final String TENANT = "patron_pin_integration_tests";
  private static UsersClient usersClient;
  private static PatronPinClient patronPinClient;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, TENANT, token);

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    usersClient = new UsersClient(okapiUrl, headers);
    patronPinClient = new PatronPinClient(okapiUrl.asURI(), headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers)
      .onComplete(context.succeedingThenComplete()));
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
    DBTestUtil.deleteFromTable(vertx, "patronpin", TENANT);
  }

  @AfterAll
  public static void after() {
    kafkaCluster.stop();
  }
}
