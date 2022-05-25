package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static org.hamcrest.CoreMatchers.is;

import java.net.URI;

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

  public AddressType getAddressType(String id) {
    return attemptToGetAddressType(id)
      .statusCode(is(HTTP_OK))
      .extract().as(AddressType.class);
  }

  public ValidatableResponse attemptToGetAddressType(String id) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .get("/addresstypes/{id}", id)
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

  public void updateAddressType(@NonNull AddressType addressType) {
    given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(addressType)
      .put("/addresstypes/{id}", addressType.getId())
      .then()
      .statusCode(is(HTTP_NO_CONTENT));
  }

  public void deleteAddressType(String id) {
    attemptToDeleteAddressType(id)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToDeleteAddressType(String id) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .delete("/addresstypes/{id}", id)
      .then();
  }

  public void deleteAllAddressTypes() {
      final var groups = getAllAddressTypes();

      groups.getAddressTypes().forEach(addressType -> deleteAddressType(addressType.getId()));
  }
}
