package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static org.apache.http.HttpStatus.SC_OK;
import static org.folio.test.util.TokenTestUtil.generateToken;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.ValidatableResponse;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldCollection;
import org.folio.rest.jaxrs.model.CustomFieldOptionStatistic;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.PutCustomFieldCollection;
import org.folio.support.User;

public class CustomFieldsClient {
  private final RestAssuredCollectionApiClient<CustomField, CustomFieldCollection> client;

  public CustomFieldsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/custom-fields"),
      defaultHeaders, CustomField.class, CustomFieldCollection.class);
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
    return createCustomField(customField, getOkapiUserHeaders(creatingUser));
  }

  public CustomField createCustomField(CustomField customField, Header userId, Header token) {
    return createCustomField(customField, getOkapiUserHeaders(userId, token));
  }

  public CustomField createCustomField(CustomField customField, Headers headers) {
    return client.initialSpecification()
      .headers(headers)
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
    updateCustomField(customField, getOkapiUserHeaders(updatingUser));
  }

  public void updateCustomField(CustomField customField, Header userId, Header token) {
    updateCustomField(customField, getOkapiUserHeaders(userId, token));
  }

  public void updateCustomField(CustomField customField, Headers headers) {
    client.initialSpecification()
      .headers(headers)
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
  public void updateCustomFields(PutCustomFieldCollection bulkRequest, User updatingUser) {
    updateCustomFields(bulkRequest, getOkapiUserHeaders(updatingUser));
  }

  public void updateCustomFields(PutCustomFieldCollection bulkRequest, Header userId, Header token) {
    updateCustomFields(bulkRequest, getOkapiUserHeaders(userId, token));
  }

  public void updateCustomFields(PutCustomFieldCollection bulkRequest, Headers headers) {
    client.initialSpecification()
      .headers(headers)
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
      .headers(getOkapiUserHeaders(creatingUser))
      .contentType(JSON)
      .when()
      .body(customField)
      .post()
      .then();
  }

  public CustomFieldCollection getCustomFields(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public CustomFieldCollection getAllCustomFields() {
    return client.getAllRecords();
  }

  public void deleteCustomField(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteCustomField(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteAllCustomFields() {
    var customFields = getAllCustomFields();

    customFields.getCustomFields()
      .forEach(field -> deleteCustomField(field.getId()));
  }

  public CustomFieldStatistic getCustomFieldStats(String customFieldId) {
    return attemptGetCustomFieldStats(customFieldId)
      .statusCode(SC_OK)
      .extract()
      .as(CustomFieldStatistic.class);
  }

  public ValidatableResponse attemptGetCustomFieldStats(String customFieldId) {
    return client.initialSpecification()
      .contentType(JSON)
      .when()
      .get("/{id}/stats", customFieldId)
      .then();
  }

  public CustomFieldOptionStatistic getCustomFieldOptionStats(String cfId, String optionId) {
    return attemptGetCustomFieldOptionStats(cfId, optionId)
      .statusCode(SC_OK)
      .extract()
      .as(CustomFieldOptionStatistic.class);
  }

  public ValidatableResponse attemptGetCustomFieldOptionStats(String cfId, String optionId) {
    return client.initialSpecification()
      .contentType(JSON)
      .when()
      .get("/{cfId}/options/{optionId}/stats", cfId, optionId)
      .then();
  }

  private static Headers getOkapiUserHeaders(User user) {
    if (user == null) {
      return new Headers();
    }

    return new Headers(
      new Header("X-Okapi-Token", generateToken(user.getUsername(), user.getId())),
      new Header("X-Okapi-User-Id", user.getId())
    );
  }

  private static Headers getOkapiUserHeaders(Header userIdHeader, Header tokenHeader) {
    return new Headers(userIdHeader, tokenHeader);
  }
}
