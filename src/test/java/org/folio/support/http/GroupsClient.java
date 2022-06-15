package org.folio.support.http;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;

import org.folio.support.Group;
import org.folio.support.Groups;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class GroupsClient {
  private final RestAssuredCollectionApiClient<Group, Groups> client;

  public GroupsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/groups"),
      defaultHeaders, Group.class, Groups.class);
  }

  public Group createGroup(@NonNull Group group) {
    return client.createRecord(group);
  }

  public ValidatableResponse attemptToCreateGroup(@NonNull Group group) {
    return client.attemptToCreateRecord(group);
  }

  public void deleteGroup(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteGroup(String id) {
    return client.attemptToDeleteRecord(id);
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
    client.attemptToUpdateRecord(group.getId(), group)
      .statusCode(HTTP_NO_CONTENT);
  }
}
