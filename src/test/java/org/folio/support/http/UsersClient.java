package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.folio.support.User;
import org.folio.support.Users;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import lombok.NonNull;

public class UsersClient {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;
  public UsersClient(URI baseUri, OkapiHeaders defaultHeaders) {
    requestSpecification = new RequestSpecBuilder()
      .setBaseUri(baseUri)
      .addHeader("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .addHeader("X-Okapi-Token", defaultHeaders.getToken())
      .addHeader("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .setAccept("application/json, text/plain")
      .build();

    config = RestAssuredConfig.newConfig()
      .objectMapperConfig(new ObjectMapperConfig(ObjectMapperType.JACKSON_2))
      .logConfig(new LogConfig().enableLoggingOfRequestAndResponseIfValidationFails());
  }

  public User createUser(@NonNull User user) {
    return attemptToCreateUser(user)
      .statusCode(HTTP_CREATED)
      .extract().as(User.class);
  }

  public ValidatableResponse attemptToCreateUser(@NonNull User user) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(user)
      .post("/users")
      .then();
  }

  public User getUser(String id) {
    return attemptToGetUser(id)
      .statusCode(HTTP_OK)
      .extract().as(User.class);
  }

  public ValidatableResponse attemptToGetUser(String id) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .get("/users/{id}", Map.of("id", id))
      .then();
  }

  public Users getUsers(String cqlQuery) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get("/users")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Users.class);
  }

  public void deleteUsers(String cqlQuery) {
    given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .delete("/users")
      .then()
      .statusCode(204);
  }

  public void deleteAllUsers() {
    deleteUsers("cql.allRecords=1");
  }

  public void updateUser(@NonNull User user) {
    attemptToUpdateUser(user)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToUpdateUser(@NonNull User user) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(user)
      .put("/users/{id}", Map.of("id", user.getId()))
      .then();
  }
}
