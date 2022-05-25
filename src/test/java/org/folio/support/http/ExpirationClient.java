package org.folio.support.http;

import static io.restassured.RestAssured.given;

import java.net.URI;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;

public class ExpirationClient {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;

  public ExpirationClient(URI baseUri, OkapiHeaders defaultHeaders) {
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

  public ValidatableResponse attemptToTriggerExpiration(String tenantId) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .header("X-Okapi-Tenant", tenantId)
      .when()
      .post("/users/expire/timer")
      .then();
  }
}
