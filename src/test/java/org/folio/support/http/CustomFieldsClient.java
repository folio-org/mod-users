package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static org.folio.test.util.TokenTestUtil.generateToken;

import org.folio.support.CustomField;
import org.folio.support.CustomFields;
import org.folio.support.PutCustomFieldsRequest;
import org.folio.support.User;

import io.restassured.response.ValidatableResponse;

public class CustomFieldsClient {
  private final RestAssuredCollectionApiClient<CustomField, CustomFields> client;

  public CustomFieldsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/custom-fields"),
      defaultHeaders, CustomField.class, CustomFields.class);
  }

  /**
   * Creates a custom field
   *
   * @param customField the custom field to create
   * @param creatingUser the user needed to create the custom field,
   *                     as custom fields specifically requires that a user exist
   *                     and be referenced when creating a custom field this way
   * @return the created custom field
   */
  public CustomField createCustomField(CustomField customField, User creatingUser) {
    return client.initialSpecification()
      .header("X-Okapi-Token", generateToken(creatingUser.getUsername(), creatingUser.getId()))
      .header("X-Okapi-User-Id", creatingUser.getId())
      .contentType(JSON)
      .when()
      .body(customField)
      .post()
      .then()
      .statusCode(HTTP_CREATED)
      .extract().as(CustomField.class);
  }

  /**
   * Update an existing custom field
   *
   * @param customField the custom field to update
   * @param updatingUser the user needed to update the custom field,
   *                     as custom fields specifically requires that a user exist
   *                     and be referenced when creating a custom field this way
   */
  public void updateCustomField(CustomField customField, User updatingUser) {
    client.initialSpecification()
      .header("X-Okapi-Token", generateToken(updatingUser.getUsername(), updatingUser.getId()))
      .header("X-Okapi-User-Id", updatingUser.getId())
      .contentType(JSON)
      .when()
      .body(customField)
      .put("/{id}", customField.getId())
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }

  /**
   * Update an custom fields in bulk
   *
   * @param bulkRequest the custom fields request to create/update
   * @param updatingUser the user needed to update the custom field,
   *                     as custom fields specifically requires that a user exist
   *                     and be referenced when creating a custom field this way
   */
  public void updateCustomFields(PutCustomFieldsRequest bulkRequest, User updatingUser) {
    client.initialSpecification()
      .header("X-Okapi-Token", generateToken(updatingUser.getUsername(), updatingUser.getId()))
      .header("X-Okapi-User-Id", updatingUser.getId())
      .contentType(JSON)
      .when()
      .body(bulkRequest)
      .put()
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }

  public CustomField getCustomField(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetCustomField(String id) {
    return client.attemptToGetRecord(id);
  }

  public ValidatableResponse attemptToCreateCustomField(CustomField customField, User creatingUser) {
    return client.initialSpecification()
      .header("X-Okapi-Token", generateToken(creatingUser.getUsername(), creatingUser.getId()))
      .header("X-Okapi-User-Id", creatingUser.getId())
      .contentType(JSON)
      .when()
      .body(customField)
      .post()
      .then();
  }

  public CustomFields getCustomFields(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  private CustomFields getAllCustomFields() {
    return client.getAllRecords();
  }

  public void deleteCustomField(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteCustomField(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteAllCustomFields() {
    final CustomFields customFields = getAllCustomFields();

    customFields.getCustomFields()
      .forEach(field -> deleteCustomField(field.getId()));
  }
}
