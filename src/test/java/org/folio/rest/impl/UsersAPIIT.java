
package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.net.URI;
import java.util.Base64;
import java.util.List;
import java.util.UUID;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.specification.RequestSpecification;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
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
    String id1 = UUID.randomUUID().toString();
    String id2 = UUID.randomUUID().toString();
    String id3 = UUID.randomUUID().toString();
    createUser(id1, "1234");
    createUser(id2, "201");
    createUser(id3, "1999");

    deleteUsersByUsername("1*");

    userExists(id2);
    userDoesntExist(id1);
    userDoesntExist(id3);
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
  void postPatronPin() {
    String id1 = UUID.randomUUID().toString();
    String id2 = UUID.randomUUID().toString();
    String id3 = UUID.randomUUID().toString();

    createUser(id1, "apple");
    createUser(id2, "banana");
    createUser(id3, "cherry");

    postPatronPinOK(id1, "1468");
    postPatronPinOK(id2, "ThisIsALonger1234PinWithSomeNumbers");
    postPatronPinOK(id3, "7778");

    // Update the patron pin for cherry
    postPatronPinOK(id3, "7777");

    pinIsCorrect(id1, "1468");
    pinIsIncorrect(id1, "1467");
    pinIsIncorrect(id2, "1111");
    pinIsIncorrect(id3, "7778");
    pinIsCorrect(id3, "7777");

    deletePatronPinOK(id1);
  }

  static RequestSpecification given() {
    return RestAssured
        .given()
        .header("X-Okapi-Tenant", TENANT)
        .header("X-Okapi-Token", TOKEN)
        .header("X-Okapi-Url", baseUrl)
        .contentType(ContentType.JSON)
        .accept("application/json,text/plain");
  }

  void userExists(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(200);
  }

  void userDoesntExist(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(404);
  }

  void createUser(String id, String username) {
    usersClient.createUser(User.builder()
      .id(id)
      .username(username)
      .build());
  }

  void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
  }

  void postPatronPinOK(String id, String pin) {
    given().
    when().
      body(new JsonObject().put("id",  id).put("pin", pin).encode()).
      post("/patron-pin").
    then().
      statusCode(201);
  }

  void deletePatronPinOK(String id) {
    given().
    when().
      body(new JsonObject().put("id",  id).encode()).
      delete("/patron-pin").
    then().
      statusCode(200);
  }

  void pinIsCorrect(String id, String pin) {
    given().
    when().
      body(new JsonObject().put("id",  id).put("pin", pin).encode()).
      post("/patron-pin/verify").
    then().
      statusCode(200);
  }

  void pinIsIncorrect(String id, String pin) {
    given().
    when().
      body(new JsonObject().put("id",  id).put("pin", pin).encode()).
      post("/patron-pin/verify").
    then().
      statusCode(422);
  }
}
