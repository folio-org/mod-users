package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.folio.support.Group;
import org.folio.support.Groups;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class GroupsClient {

  private final RestAssuredClient<Group> client;

  public GroupsClient(URI baseUri, OkapiHeaders defaultHeaders) {
    client = new RestAssuredClient<>(baseUri, defaultHeaders);
  }

  public Group createGroup(@NonNull Group group) {
    return client.createRecord("/groups", group, Group.class);
  }

  public ValidatableResponse attemptToCreateGroup(@NonNull Group group) {
    return client.attemptToCreateRecord("/groups", group);
  }

  public Groups getAllGroups() {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .get("/groups")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Groups.class);
  }

  public void deleteGroup(String id) {
    attemptToDeleteGroup(id)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToDeleteGroup(String id) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .delete("/groups/{id}", Map.of("id", id))
      .then();
  }

  public void deleteAllGroups() {
    final var groups = getAllGroups();

    groups.getUsergroups().forEach(group -> deleteGroup(group.getId()));
  }

  public Group getGroup(String id) {
    return client.getRecord("/groups/{id}", id, Group.class);
  }

  public ValidatableResponse attemptToGetGroup(String id) {
    return client.attemptToGetRecord("/groups/{id}", id);
  }

  public Groups findGroups(String cqlQuery) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get("/groups")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Groups.class);
  }

  public void updateGroup(@NonNull Group group) {
    given()
      .spec(client.requestSpecification)
      .contentType(JSON)
      .when()
      .body(group)
      .put("/groups/{id}", Map.of("id", group.getId()))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }
}
