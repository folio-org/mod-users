package org.folio.support.http;

import org.folio.support.CustomField;
import org.folio.support.CustomFields;

public class CustomFieldsClient {
  private final RestAssuredCollectionApiClient<CustomField, CustomFields> client;

  public CustomFieldsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/custom-fields"),
      defaultHeaders, CustomField.class, CustomFields.class);
  }

  public CustomFields getCustomFields(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

}
