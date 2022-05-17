package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;

import java.net.URI;

import org.folio.support.User;

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

  public void deleteAllUsers() {
    given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .param("query", "cql.allRecords=1")
      .delete("/users")
      .then()
      .statusCode(204);
  }
}
