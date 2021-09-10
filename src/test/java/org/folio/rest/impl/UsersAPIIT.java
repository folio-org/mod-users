
package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.not;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.specification.RequestSpecification;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

/**
 * Most old UsersAPI tests are in deprecated org.folio.moduserstest.RestVerticleIT and
 * should be moved here.
 */
class UsersAPIIT {
  static final String TENANT = "usersapiit";
  static final String TOKEN = "header." + Base64.getEncoder().encodeToString("{}".getBytes()) + ".signature";
  static Vertx vertx;
  static String baseUrl;

  private static final Logger log = LogManager.getLogger(UsersAPIIT.class);

  @BeforeAll
  static void beforeAll() {
    vertx = Vertx.vertx();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.port = NetworkUtils.nextFreePort();
    baseUrl = "http://localhost:" + RestAssured.port;
    deploy();
    deleteTenantIgnore();  // remove left over from previous test run
    postTenant();
  }

  @AfterAll
  static void afterAll() {
    vertx.close();
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

  static void deploy() {
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject()
        .put("http.port", RestAssured.port));
    CompletableFuture<String> future = new CompletableFuture<>();
    vertx.deployVerticle(new RestVerticle(), options, handler -> {
      if (handler.succeeded()) {
        future.complete(handler.result());
      } else {
        future.completeExceptionally(handler.cause());
      }
    });
    try {
      future.get();
    } catch (InterruptedException | ExecutionException e) {
      throw new RuntimeException(e);
    }
  }

  static void deleteTenantIgnore() {
    given().
    when().delete("/_/tenant");
    // ignore any error
  }

  static TenantAttributes tenantAttributes() {
    List<Parameter> parameters = new ArrayList<>();
    parameters.add(new Parameter().withKey("loadReference").withValue("true"));
    parameters.add(new Parameter().withKey("loadSample").withValue("true"));
    return new TenantAttributes()
        .withModuleTo("mod-users-9999999.0.0")
        .withParameters(parameters);
  }

  static void postTenant() {
    String id = given().body(tenantAttributes()).
    when().post("/_/tenant").
    then().statusCode(201).
    extract().
    path("id");
    Boolean complete = given().when().get("/_/tenant/" + id + "?wait=60000")
    .then().statusCode(200).extract().path("complete");
    Assert.assertTrue(complete);
    //if something went wrong internally with client setup,
    //there will be an error attribute in the response body
    given().when().get("/_/tenant/" + id).then().statusCode(200).body("$", not(hasKey("error")));
  }

  static void userExists(String id) {
    given().
    when().get("/users/" + id).
    then().statusCode(200);
  }

  static void userDoesntExist(String id) {
    given().
    when().get("/users/" + id).
    then().statusCode(404);
  }

  static void postUser(String id, String username) {
    given().
    when().
      body(new JsonObject().put("id",  id).put("username", username).encode()).
      post("/users").
    then().
      statusCode(201);
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
    String id1 = "114b14e8-422d-429a-b89e-b55c439b3b21";
    String id2 = "22531038-08b8-4c71-a72c-9d48f63f3682";
    String id3 = "13836e0e-4331-4386-a8be-2345eece6de3";
    postUser(id1, id1);
    postUser(id2, id2);
    postUser(id3, id3);
    given().
    when().
      param("query", "username == \"1*\"").
      delete("/users").
    then().
      statusCode(204);
    userExists(id2);
    userDoesntExist(id1);
    userDoesntExist(id3);
  }
}
