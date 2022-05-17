package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.util.Map;

import org.folio.support.Group;
import org.folio.support.Groups;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;
import lombok.SneakyThrows;

public class GroupsClient {
  private final OkapiHeaders defaultHeaders;

  public GroupsClient(OkapiHeaders defaultHeaders) {
    this.defaultHeaders = defaultHeaders;
  }

  public Group createGroup(@NonNull Group group) {
    return attemptToCreateGroup(group)
      .statusCode(HTTP_CREATED)
      .extract().as(Group.class);
  }

  @SneakyThrows
  public ValidatableResponse attemptToCreateGroup(@NonNull Group group) {
    return given()
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
      .contentType(JSON)
      .when()
      .body(new ObjectMapper().writeValueAsString(group))
      .post("/groups")
      .then();
  }

  public Groups getAllGroups() {
    return given()
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
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
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
      .when()
      .delete("/groups/{id}", Map.of("id", id))
      .then();
  }

  public void deleteAllGroups() {
    final var groups = getAllGroups();

    groups.getUsergroups().forEach(group -> deleteGroup(group.getId()));
  }

  public Group getGroup(String id) {
    return attemptToGetGroup(id)
      .statusCode(HTTP_OK)
      .extract().as(Group.class);
  }

  public ValidatableResponse attemptToGetGroup(String id) {
    return given()
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
      .when()
      .get("/groups/{id}", Map.of("id", id))
      .then();
  }

  public Groups findGroups(String cqlQuery) {
    return given()
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
      .when()
      .queryParam("query", cqlQuery)
      .get("/groups")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Groups.class);
  }

  @SneakyThrows
  public void updateGroup(@NonNull Group group) {
    given()
      .header("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .header("X-Okapi-Token", defaultHeaders.getToken())
      .header("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .accept("application/json, text/plain")
      .contentType(JSON)
      .when()
      .body(new ObjectMapper().writeValueAsString(group))
      .put("/groups/{id}", Map.of("id", group.getId()))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }
}
