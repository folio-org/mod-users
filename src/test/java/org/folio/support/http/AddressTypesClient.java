package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;

import org.folio.support.AddressTypes;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.specification.RequestSpecification;

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
}
