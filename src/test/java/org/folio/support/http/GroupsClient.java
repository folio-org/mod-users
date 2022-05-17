package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.util.Map;

import org.folio.support.Groups;

import io.restassured.response.ValidatableResponse;

public class GroupsClient {
  private final OkapiHeaders defaultHeaders;

  public GroupsClient(OkapiHeaders defaultHeaders) {
    this.defaultHeaders = defaultHeaders;
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
}
