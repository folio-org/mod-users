package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;

import java.net.URI;

import org.folio.support.PatronPin;

import io.restassured.response.ValidatableResponse;

public class PatronPinClient {
  private final RestAssuredConfiguration configuration;

  public PatronPinClient(URI baseUri, OkapiHeaders defaultHeaders) {
    configuration = new RestAssuredConfiguration(baseUri, defaultHeaders);
  }

  public void assignPatronPin(String userId, String pin) {
    configuration.initialSpecification()
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

  public ValidatableResponse attemptToVerifyPatronPin(String userId, String pin) {
    return configuration.initialSpecification()
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
    configuration.initialSpecification()
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
