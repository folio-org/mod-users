
package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.net.URI;
import java.util.Base64;
import java.util.List;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.PatronPinClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import io.restassured.RestAssured;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

/**
 * Most old UsersAPI tests are in deprecated org.folio.moduserstest.RestVerticleIT and
 * should be moved here.
 */
@ExtendWith(VertxExtension.class)
class UsersAPIIT {
  static final String HOME_ADDRESS_TYPE_ID = "93d3d88d-499b-45d0-9bc7-ac73c3a19880";
  static final String ClAIM_ADDRESS_TYPE_ID = "b6f4d1c6-0dfa-463c-9534-f49c4f0ae090";
  static final String TENANT = "usersapiit";
  static final String TOKEN = "header." + Base64.getEncoder().encodeToString("{}".getBytes()) + ".signature";
  static String baseUrl;
  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static PatronPinClient patronPinClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final var port = NetworkUtils.nextFreePort();

    RestAssured.port = port;
    baseUrl = "http://localhost:" + port;

    final var headers = new OkapiHeaders("http://localhost:" + port,
      TENANT, TOKEN);

    usersClient = new UsersClient(new URI("http://localhost:" + port), headers);
    groupsClient = new GroupsClient(new URI("http://localhost:" + port), headers);
    patronPinClient = new PatronPinClient(new URI("http://localhost:" + port), headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .onComplete(context.succeeding(res -> module.enableModule(headers,
          true, false)
        .onComplete(context.succeedingThenComplete())));
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
  }

  @Test
  void canGetPatronGroupFacetsForUsers() {
    final var alphaGroup = groupsClient.createGroup(Group.builder()
      .group("Alpha group")
      .build());

    var zebraGroup = groupsClient.createGroup(Group.builder()
      .group("Zebra group")
      .build());

    usersClient.createUser(User.builder()
      .username("julia")
      .patronGroup(alphaGroup.getId()).build());

    usersClient.createUser(User.builder()
      .username("alex")
      .patronGroup(zebraGroup.getId()).build());

    usersClient.createUser(User.builder()
      .username("steven")
      .patronGroup(zebraGroup.getId()).build());

    final var patronGroupFacets = usersClient.getPatronGroupFacets();

    assertThat(patronGroupFacets.getTotalRecords(), is(3));
    assertThat(patronGroupFacets.getFacetCount(zebraGroup.getId()), is(2));
    assertThat(patronGroupFacets.getFacetCount(alphaGroup.getId()), is(1));
  }

  @Test
  void deleteMultipleUsersUsingCQL() {
    final var user1 = createUser("1234");
    final var user2 = createUser("201");
    final var user3 = createUser("1999");

    deleteUsersByUsername("1*");

    userExists(user2.getId());
    userDoesntExist(user1.getId());
    userDoesntExist(user3.getId());
  }

  @Test
  void canCreateUser() {
    final var userToCreate = User.builder()
      .username("julia")
      .personal(Personal.builder()
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build(),
          Address.builder().addressTypeId(ClAIM_ADDRESS_TYPE_ID).build()))
        .build())
      .build();

    final var createdUser = usersClient.createUser(userToCreate);

    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getUsername(), is("julia"));

    final var personal = createdUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getAddresses().size(), is(2));
  }

  @Test
  void cannotCreateUserWithMultipleAddressesOfSameType() {
    final var userWithMultipleAddresses = User.builder()
      .username("julia")
      .personal(Personal.builder()
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build(),
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build()))
        .build())
      .build();

    usersClient.attemptToCreateUser(userWithMultipleAddresses)
      .statusCode(400)
      .body(is("Users are limited to one address per addresstype"));
  }

  @Test
  void canFindActiveUsers() {
    usersClient.createUser(User.builder()
      .username("steve")
      .active(true)
      .build());

    usersClient.createUser(User.builder()
      .username("joanne")
      .active(false)
      .build());

    usersClient.createUser(User.builder()
      .username("jenna")
      .active(true)
      .build());

    final var activeUsers = usersClient.getUsers("active=true");

    assertThat(activeUsers.getTotalRecords(), is(2));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUser() {
    usersClient.createUser("julia");

    final var response = usersClient.attemptToCreateUser(User.builder()
      .username("julia").build());

    response.statusCode(is(422));

    final var errors = response.extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameIdAsExistingUser() {
    final var existingUser = usersClient.createUser("julia");

    final var errors = usersClient.attemptToCreateUser(User.builder()
      .id(existingUser.getId())
      .username("steve")
      .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("User with this id already exists"));
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
    patronPinClient.verifyPatronPin(user.getId(), pin)
      .statusCode(is(200));
  }

  private void enteredPinIsInvalid(User user, String pin) {
    patronPinClient.verifyPatronPin(user.getId(), pin)
      .statusCode(is(422));
  }

  void userExists(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(200);
  }

  void userDoesntExist(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(404);
  }

  User createUser(String username) {
    return usersClient.createUser(User.builder()
      .username(username)
      .build());
  }

  private void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
  }
}
