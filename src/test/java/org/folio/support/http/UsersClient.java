package org.folio.support.http;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import org.folio.support.User;
import org.folio.support.Users;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class UsersClient {
  private final RestAssuredCollectionApiClient<User, Users> client;

  public UsersClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/users"),
      defaultHeaders, User.class, Users.class);
  }

  public User createUser(@NonNull User user) {
    return client.createRecord(user);
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
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetUser(String id) {
    return client.attemptToGetRecord(id);
  }

  public Users getUsers(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public ValidatableResponse attemptToGetUsers(String cqlQuery) {
    return client.attemptToGetRecords(cqlQuery);
  }

  public Users getAllUsers() {
    return client.getAllRecords();
  }

  public Users getPatronGroupFacets() {
    return client.initialSpecification()
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
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteUser(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteUsers(String cqlQuery) {
    client.deleteRecords(cqlQuery);
  }

  public ValidatableResponse attemptToDeleteUsers(String cqlQuery) {
    return client.attemptToDeleteRecords(cqlQuery);
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
    return client.attemptToUpdateRecord(id, user);
  }
}
