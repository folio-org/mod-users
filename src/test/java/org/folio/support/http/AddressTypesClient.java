package org.folio.support.http;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static org.hamcrest.CoreMatchers.is;

import org.folio.support.AddressType;
import org.folio.support.AddressTypes;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class AddressTypesClient {
  private final RestAssuredCollectionApiClient<AddressType, AddressTypes> client;

  public AddressTypesClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/addresstypes"),
      defaultHeaders, AddressType.class, AddressTypes.class);
  }

  public AddressType createAddressType(@NonNull AddressType addressType) {
    return client.createRecord(addressType);
  }

  public ValidatableResponse attemptToCreateAddressType(
    @NonNull AddressType addressType) {

    return client.attemptToCreateRecord(addressType);
  }

  public AddressType getAddressType(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetAddressType(String id) {
    return client.attemptToGetRecord(id);
  }

  public AddressTypes getAllAddressTypes() {
    return client.getAllRecords();
  }

  public void updateAddressType(@NonNull AddressType addressType) {
    client.attemptToUpdateRecord(addressType.getId(), addressType)
      .statusCode(is(HTTP_NO_CONTENT));
  }

  public void deleteAddressType(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteAddressType(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteAllAddressTypes() {
      final var groups = getAllAddressTypes();

      groups.getAddressTypes().forEach(addressType -> deleteAddressType(addressType.getId()));
  }
}
