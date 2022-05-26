package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.util.Map;

import org.folio.support.User;
import org.folio.support.Users;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class UsersClient {
  private final RestAssuredClient<User> client;

  public UsersClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredClient<>(okapiUrl.asURI("/users"), defaultHeaders);
  }

  public User createUser(@NonNull User user) {
    return client.createRecord("", user, User.class);
  }

  public User createUser(String username) {
    return createUser(User.builder()
      .username(username)
      .build());
  }

  public ValidatableResponse attemptToCreateUser(@NonNull User user) {
    return client.attemptToCreateRecord("", user);
  }

  public User getUser(String id) {
    return client.getRecord("/{id}", id, User.class);
  }

  public ValidatableResponse attemptToGetUser(String id) {
    return client.attemptToGetRecord("/{id}", id);
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
      .get()
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
      .get()
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
      .delete("/{id}", Map.of("id", id))
      .then();
  }

  public void deleteUsers(String cqlQuery) {
    given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .delete()
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
      .put("/{id}", Map.of("id", id))
      .then();
  }
}
