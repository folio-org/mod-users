package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.folio.support.AddressType;
import org.folio.support.AddressTypes;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import lombok.NonNull;

public class AddressTypesClient {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;
  public AddressTypesClient(URI baseUri, OkapiHeaders defaultHeaders) {
    requestSpecification = new RequestSpecBuilder()
      .setBaseUri(baseUri)
      .addHeader("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .addHeader("X-Okapi-Token", defaultHeaders.getToken())
      .addHeader("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .setAccept("application/json, text/plain")
      .build();

    config = RestAssuredConfig.newConfig()
      .objectMapperConfig(new ObjectMapperConfig(ObjectMapperType.JACKSON_2))
      .logConfig(new LogConfig().enableLoggingOfRequestAndResponseIfValidationFails());
  }

  public AddressType createAddressType(@NonNull AddressType addressType) {
    return attemptToCreateAddressType(addressType)
      .statusCode(HTTP_CREATED)
      .extract().as(AddressType.class);
  }

  public ValidatableResponse attemptToCreateAddressType(
    @NonNull AddressType addressType) {

    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(addressType)
      .post("/addresstypes")
      .then();
  }

  public AddressTypes getAddressTypes(String cqlQuery) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get("/addresstypes")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(AddressTypes.class);
  }

  public AddressTypes getAllAddressTypes() {
    return getAddressTypes("cql.AllRecords=1");
  }

  private void deleteAddressType(String id) {
    given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .delete("/addresstypes/{id}", Map.of("id", id))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }

  public void deleteAllAddressTypes() {
      final var groups = getAllAddressTypes();

      groups.getAddressTypes().forEach(addressType -> deleteAddressType(addressType.getId()));
  }
}
