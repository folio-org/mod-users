package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.jetbrains.annotations.NotNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;

public class RestAssuredClient<Record, Collection> {
  final RequestSpecification requestSpecification;
  final RestAssuredConfig config;
  private final Class<Record> recordDeserializesTo;
  private final Class<Collection> collectionDeserializesTo;

  RestAssuredClient(URI baseUri, OkapiHeaders defaultHeaders,
    Class<Record> recordDeserializesTo,
    Class<Collection> collectionDeserializesTo) {

    this.requestSpecification = new RequestSpecBuilder()
      .setBaseUri(baseUri)
      .addHeader("X-Okapi-Tenant", defaultHeaders.getTenantId())
      .addHeader("X-Okapi-Token", defaultHeaders.getToken())
      .addHeader("X-Okapi-Url", defaultHeaders.getOkapiUrl())
      .setAccept("application/json, text/plain")
      .build();

    this.config = RestAssuredConfig.newConfig()
      .logConfig(new LogConfig().enableLoggingOfRequestAndResponseIfValidationFails())
      .objectMapperConfig(new ObjectMapperConfig(ObjectMapperType.JACKSON_2)
        .jackson2ObjectMapperFactory((type, s) -> {
          final var mapper = new ObjectMapper();

          mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
          mapper.registerModule(new JavaTimeModule());

          return mapper;
        }));

    this.recordDeserializesTo = recordDeserializesTo;
    this.collectionDeserializesTo = collectionDeserializesTo;
  }

  Record createRecord(@NotNull Record user) {
    return attemptToCreateRecord(user)
      .statusCode(HTTP_CREATED)
      .extract().as(recordDeserializesTo);
  }

  ValidatableResponse attemptToCreateRecord(@NotNull Record record) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(record)
      .post()
      .then();
  }

  Record getRecord(String id) {
    return attemptToGetRecord(id)
      .statusCode(HTTP_OK)
      .extract().as(recordDeserializesTo);
  }

  ValidatableResponse attemptToGetRecord(String id) {
    return given()
      .config(this.config)
      .spec(this.requestSpecification)
      .when()
      .get("/{id}", Map.of("id", id))
      .then();
  }

  Collection getRecords(String cqlQuery) {
    return attemptToGetRecords(cqlQuery)
      .statusCode(HTTP_OK)
      .extract().as(collectionDeserializesTo);
  }

  Collection getAllRecords() {
    return getRecords("cql.AllRecords=1");
  }

  ValidatableResponse attemptToGetRecords(String cqlQuery) {
    return given()
      .config(this.config)
      .spec(this.requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get()
      .then();
  }
}
