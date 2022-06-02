package org.folio.support.http;

import static io.restassured.RestAssured.given;

import java.net.URI;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.specification.RequestSpecification;

class RestAssuredConfiguration {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;

  public RestAssuredConfiguration(URI baseUri, OkapiHeaders defaultHeaders) {
    requestSpecification = new RequestSpecBuilder()
      .setBaseUri(baseUri)
      .addHeader("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .addHeader("X-Okapi-Token", defaultHeaders.getToken())
      .addHeader("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .setAccept("application/json, text/plain")
      .build();

    config = RestAssuredConfig.newConfig()
      .logConfig(
        new LogConfig().enableLoggingOfRequestAndResponseIfValidationFails())
      .objectMapperConfig(new ObjectMapperConfig(ObjectMapperType.JACKSON_2)
        .jackson2ObjectMapperFactory((type, s) -> {
          final var mapper = new ObjectMapper();

          mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
          mapper.registerModule(new JavaTimeModule());

          return mapper;
        }));
  }

  RequestSpecification initialSpecification() {
    return given()
      .config(config)
      .spec(requestSpecification);
  }
}
