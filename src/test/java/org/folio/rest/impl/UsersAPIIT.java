
package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;

import java.util.Base64;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.http.OkapiHeaders;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testcontainers.shaded.com.fasterxml.jackson.databind.ObjectMapper;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import lombok.SneakyThrows;

/**
 * Most old UsersAPI tests are in deprecated org.folio.moduserstest.RestVerticleIT and
 * should be moved here.
 */
class UsersAPIIT {
  static final String HOME_ADDRESS_TYPE_ID = "93d3d88d-499b-45d0-9bc7-ac73c3a19880";
  static final String ClAIM_ADDRESS_TYPE_ID = "b6f4d1c6-0dfa-463c-9534-f49c4f0ae090";
  static final String TENANT = "usersapiit";
  static final String TOKEN = "header." + Base64.getEncoder().encodeToString("{}".getBytes()) + ".signature";
  static Vertx vertx;
  static String baseUrl;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    vertx = Vertx.vertx();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final var port = NetworkUtils.nextFreePort();

    RestAssured.port = port;
    baseUrl = "http://localhost:" + port;

    final var headers = new OkapiHeaders("http://localhost:" + port,
      TENANT, TOKEN);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers, true, true))
      .toCompletionStage()
      .toCompletableFuture()
      .get(30, TimeUnit.SECONDS);
  }

  @AfterAll
  static void afterAll() {
    vertx.close();
  }

  @Disabled("fails, bug")  // https://issues.folio.org/browse/UIU-1562  https:/issues.folio.org/browse/RMB-722
  @Test
  void facetsLimit0() {
    // The UI uses this to show the number per patron group in /settings/users/groups
    facets(0);
  }

  @Test
  void facetsLimit1() {
    facets(1);
  }

  @Test
  void deleteMultipleUsersUsingCQL() {
    String id1 = UUID.randomUUID().toString();
    String id2 = UUID.randomUUID().toString();
    String id3 = UUID.randomUUID().toString();
    postUser(id1, "1234");
    postUser(id2, "201");
    postUser(id3, "1999");

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

    createUser(userToCreate)
      .then()
      .statusCode(201);
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

    createUser(userWithMultipleAddresses)
      .then()
      .statusCode(400)
      .body(is("Users are limited to one address per addresstype"));
  }

  @Test
  void postPatronPin() {
    String id1 = UUID.randomUUID().toString();
    String id2 = UUID.randomUUID().toString();
    String id3 = UUID.randomUUID().toString();
    postUser(id1, "apple");
    postUser(id2, "banana");
    postUser(id3, "cherry");

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
    given().
    when().get("/users/" + id).
    then().statusCode(200);
  }

  void userDoesntExist(String id) {
    given().
    when().get("/users/" + id).
    then().statusCode(404);
  }

  @SneakyThrows
  private Response createUser(User userToCreate) {
    return given()
      .when()
      .body(new ObjectMapper().writeValueAsString(userToCreate))
      .post("/users");
  }

  /**
   * Create a user by calling the POST /users API.
   */
  void postUser(String id, String username) {
    given().
    when().
      body(new JsonObject().put("id",  id).put("username", username).encode()).
      post("/users").
    then().
      statusCode(201);
  }

  void deleteUsersByUsername(String username) {
    given().
    when().
      param("query", "username == \"" + username + "\"").
      delete("/users").
    then().
      statusCode(204);
  }

  void facets(int limit) {
    given().
    when().get("/users?limit=" + limit + "&facets=patronGroup:50").
    then().
      statusCode(200).
      body("resultInfo.facets[0].facetValues[0].count", is(88)).
      body("resultInfo.facets[0].facetValues[0].value", is("bdc2b6d4-5ceb-4a12-ab46-249b9a68473e")).
      body("resultInfo.facets[0].facetValues[1].count", is(81)).
      body("resultInfo.facets[0].facetValues[1].value", is("3684a786-6671-4268-8ed0-9db82ebca60b"));

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
