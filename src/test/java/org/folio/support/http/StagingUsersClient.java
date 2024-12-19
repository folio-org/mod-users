package org.folio.support.http;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.resource.StagingUsers;

import java.util.Map;

public class StagingUsersClient {
  private final RestAssuredCollectionApiClient<StagingUser, StagingUsers> client;

  public StagingUsersClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/staging-users"),
      defaultHeaders, StagingUser.class, StagingUsers.class);
  }

  public ValidatableResponse attemptToCreateStagingUser(@NonNull StagingUser stagingUser) {
    return client.attemptToCreateRecord(stagingUser);
  }

  public ValidatableResponse attemptToUpdateStagingUser(String externalSystemId, @NonNull StagingUser stagingUser) {
    return client.attemptToUpdateRecord(externalSystemId, stagingUser);
  }

  public ValidatableResponse attemptToMergeStagingUser(String stagingUserId, String userId) {
    var requestSpec = client.initialSpecification();

    if (userId != null && !userId.isEmpty()) {
      requestSpec.queryParam("userId", userId);
    }

    return requestSpec
      .when()
      .put("{id}/mergeOrCreateUser", Map.of("id", stagingUserId))
      .then();
  }

  public ValidatableResponse attemptToGetUsers(String cqlQuery) {
    return client.attemptToGetRecords(cqlQuery);
  }

}
