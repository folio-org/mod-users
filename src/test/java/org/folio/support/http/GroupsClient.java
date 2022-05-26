package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;

import java.util.Map;

import org.folio.support.Group;
import org.folio.support.Groups;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class GroupsClient {

  private final RestAssuredClient<Group, Groups> client;

  public GroupsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredClient<>(okapiUrl.asURI("/groups"),
      defaultHeaders, Group.class, Groups.class);
  }

  public Group createGroup(@NonNull Group group) {
    return client.createRecord(group);
  }

  public ValidatableResponse attemptToCreateGroup(@NonNull Group group) {
    return client.attemptToCreateRecord(group);
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
      .delete("/{id}", Map.of("id", id))
      .then();
  }

  public void deleteAllGroups() {
    final var groups = getAllGroups();

    groups.getUsergroups().forEach(group -> deleteGroup(group.getId()));
  }

  public Group getGroup(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetGroup(String id) {
    return client.attemptToGetRecord(id);
  }

  public Groups getGroups(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public Groups getAllGroups() {
    return client.getAllRecords();
  }

  public void updateGroup(@NonNull Group group) {
    given()
      .spec(client.requestSpecification)
      .contentType(JSON)
      .when()
      .body(group)
      .put("/{id}", Map.of("id", group.getId()))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }
}
