package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static org.hamcrest.CoreMatchers.is;

import org.folio.support.AddressType;
import org.folio.support.AddressTypes;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class AddressTypesClient {
  private final RestAssuredClient<AddressType> client;

  public AddressTypesClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredClient<>(okapiUrl.asURI("/addresstypes"),
      defaultHeaders);
  }

  public AddressType createAddressType(@NonNull AddressType addressType) {
    return client.createRecord("", addressType, AddressType.class);
  }

  public ValidatableResponse attemptToCreateAddressType(
    @NonNull AddressType addressType) {

    return client.attemptToCreateRecord("", addressType);
  }

  public AddressType getAddressType(String id) {
    return client.getRecord("/{id}", id, AddressType.class);
  }

  public ValidatableResponse attemptToGetAddressType(String id) {
    return client.attemptToGetRecord("/{id}", id);
  }

  public AddressTypes getAddressTypes(String cqlQuery) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get()
      .then()
      .statusCode(HTTP_OK)
      .extract().as(AddressTypes.class);
  }

  public AddressTypes getAllAddressTypes() {
    return getAddressTypes("cql.AllRecords=1");
  }

  public void updateAddressType(@NonNull AddressType addressType) {
    given()
      .config(client.config)
      .spec(client.requestSpecification)
      .contentType(JSON)
      .when()
      .body(addressType)
      .put("/{id}", addressType.getId())
      .then()
      .statusCode(is(HTTP_NO_CONTENT));
  }

  public void deleteAddressType(String id) {
    attemptToDeleteAddressType(id)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToDeleteAddressType(String id) {
    return given()
      .config(client.config)
      .spec(client.requestSpecification)
      .when()
      .delete("/{id}", id)
      .then();
  }

  public void deleteAllAddressTypes() {
      final var groups = getAllAddressTypes();

      groups.getAddressTypes().forEach(addressType -> deleteAddressType(addressType.getId()));
  }
}
