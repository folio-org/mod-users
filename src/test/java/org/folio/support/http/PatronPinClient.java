package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static org.hamcrest.CoreMatchers.is;

import java.net.URI;

import org.folio.support.PatronPin;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;

public class PatronPinClient {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;
  public PatronPinClient(URI baseUri, OkapiHeaders defaultHeaders) {
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

  public void assignPatronPin(String userId, String pin) {
    given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(PatronPin.builder()
        .id(userId)
        .pin(pin)
        .build())
      .post("/patron-pin")
      .then()
      .statusCode(201);
  }

  public ValidatableResponse verifyPatronPin(String userId, String pin) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(PatronPin.builder()
      .id(userId)
      .pin(pin)
      .build())
      .post("/patron-pin/verify")
      .then();
  }

  public void removePatronPin(String userId) {
    given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(PatronPin.builder()
        .id(userId)
        .build())
      .delete("/patron-pin")
      .then()
      .statusCode(200);
  }

}
