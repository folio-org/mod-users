package org.folio.support.http;

import io.restassured.response.ValidatableResponse;

import java.io.InputStream;
import java.util.Map;

public class UserProfilePictureClient {

  private final RestAssuredConfiguration client;

  public UserProfilePictureClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    this.client = new RestAssuredConfiguration(okapiUrl.asURI("/users/profile-picture"), defaultHeaders);
  }

  public ValidatableResponse saveUserProfilePicture(InputStream entity) {
    return client.initialSpecification()
      .contentType("application/octet-stream")
      .when()
      .body(entity)
      .post()
      .then();
  }

  public ValidatableResponse updateUserProfilePicture(String id, InputStream entity) {
    return client.initialSpecification()
      .contentType("application/octet-stream")
      .when()
      .body(entity)
      .put("/{id}", Map.of("id", id))
      .then();
  }

  public ValidatableResponse getUserProfilePicture(String id) {
    return client.initialSpecification()
      .when()
      .get("/{id}", Map.of("id", id))
      .then();
  }

  public ValidatableResponse deleteUserProfilePicture(String id) {
    return client.initialSpecification()
      .when()
      .delete("/{id}", Map.of("id", id))
      .then();
  }

}
