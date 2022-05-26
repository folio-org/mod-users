package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.folio.support.User;
import org.folio.support.Users;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class UsersClient {
  private final RestAssuredClient<User> client;

  public UsersClient(URI baseUri, OkapiHeaders defaultHeaders) {
    client = new RestAssuredClient<>(baseUri, defaultHeaders);
  }

  public User createUser(@NonNull User user) {
    return client.createRecord(user, User.class);
  }

  public User createUser(String username) {
    return createUser(User.builder()
      .username(username)
      .build());
  }

  public ValidatableResponse attemptToCreateUser(@NonNull User user) {
    return client.attemptToCreateRecord(user);
  }

  public User getUser(String id) {
    return attemptToGetUser(id)
      .statusCode(HTTP_OK)
      .extract().as(User.class);
  }

  public ValidatableResponse attemptToGetUser(String id) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .get("/users/{id}", Map.of("id", id))
      .then();
  }

  public Users getUsers(String cqlQuery) {
    return attemptToGetUsers(cqlQuery)
      .statusCode(HTTP_OK)
      .extract().as(Users.class);
  }

  public ValidatableResponse attemptToGetUsers(String cqlQuery) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get("/users")
      .then();
  }

  public Users getAllUsers() {
    return getUsers("cql.AllRecords=1");
  }

  public Users getPatronGroupFacets() {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      // Limit must be 1 as request fails with limit 0
      // https://issues.folio.org/browse/UIU-1562  https:/issues.folio.org/browse/RMB-722
      .queryParam("limit", 1)
      .queryParam("facets", "patronGroup:50")
      .get("/users")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Users.class);
  }

  public void deleteUser(String id) {
    attemptToDeleteUser(id)
      .statusCode(204);
  }

  public ValidatableResponse attemptToDeleteUser(String id) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .delete("/users/{id}", Map.of("id", id))
      .then();
  }

  public void deleteUsers(String cqlQuery) {
    given()
      .config(client.config)
      .spec(client.requestSpecification)
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
    return attemptToUpdateUser(user.getId(), user);
  }

  public ValidatableResponse attemptToUpdateUser(String id, @NonNull User user) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .contentType(JSON)
      .when()
      .body(user)
      .put("/users/{id}", Map.of("id", id))
      .then();
  }
}
