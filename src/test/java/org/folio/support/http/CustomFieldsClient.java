package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;

import org.folio.support.CustomField;
import org.folio.support.CustomFields;
import org.folio.support.User;
import org.folio.test.util.TokenTestUtil;

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
   * @param createdByUser the user needed to create it, as custom fields specifically
   *                      requires that a user exist and be referenced when creating
   *                      a custom field this way
   * @return the created custom field
   */
  public CustomField createCustomField(CustomField customField, User createdByUser) {
    return client.initialSpecification()
      .header("X-Okapi-Token", TokenTestUtil.generateToken(
      createdByUser.getUsername(), createdByUser.getId()))
      .header("X-Okapi-User-Id", createdByUser.getId())
      .contentType(JSON)
      .when()
      .body(customField)
      .post()
      .then()
      .statusCode(HTTP_CREATED)
      .extract().as(CustomField.class);
  }

  public CustomFields getCustomFields(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }
}
