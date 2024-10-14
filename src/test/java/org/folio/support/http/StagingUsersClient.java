package org.folio.support.http;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.GeneralInfo;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.resource.StagingUsers;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;

public class StagingUsersClient {
  private final RestAssuredCollectionApiClient<StagingUser, StagingUsers> client;

  public StagingUsersClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/staging-users"),
      defaultHeaders, StagingUser.class, StagingUsers.class);
  }

  public StagingUser createStagingUser(@NonNull StagingUser stagingUser) {
    return client.createRecord(stagingUser);
  }

  public StagingUser createStagingUser(String firstName, String lastName, String email) {
    StagingUser stagingUser =  new StagingUser();
    GeneralInfo generalInfo1 = new GeneralInfo();
    generalInfo1.setFirstName(firstName);
    generalInfo1.setLastName(lastName);
    stagingUser.setGeneralInfo(generalInfo1);

    ContactInfo contactInfo1 = new ContactInfo();
    contactInfo1.setEmail(email);
    stagingUser.setContactInfo(contactInfo1);
    return createStagingUser(stagingUser);
  }

  public ValidatableResponse attemptToCreateStagingUser(@NonNull StagingUser stagingUser) {
    return client.attemptToCreateRecord(stagingUser);
  }

  public StagingUser getUser(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetUser(String id) {
    return client.attemptToGetRecord(id);
  }

  public StagingUsers getStagingUsers(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public ValidatableResponse attemptToGetUsers(String cqlQuery) {
    return client.attemptToGetRecords(cqlQuery);
  }

  public StagingUsers getAllStagingUsers() {
    return client.getAllRecords();
  }

  public void deleteStagingUser(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteStagingUser(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteStagingUsers(String cqlQuery) {
    client.deleteRecords(cqlQuery);
  }

  public void deleteAllUsers() {
    deleteStagingUsers("cql.allRecords=1");
  }

  public void updateStagingUser(@NonNull StagingUser stagingUser) {
    attemptToUpdateStagingUser(stagingUser)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToUpdateStagingUser(@NonNull StagingUser stagingUser) {
    return attemptToUpdateStagingUser(stagingUser.getId(), stagingUser);
  }

  public ValidatableResponse attemptToUpdateStagingUser(String id, @NonNull StagingUser stagingUser) {
    return client.attemptToUpdateRecord(id, stagingUser);
  }
}
