package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.jetbrains.annotations.NotNull;

import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import lombok.NonNull;

public class RestAssuredCollectionApiClient<Record, Collection> {
  private final Class<Record> recordDeserializesTo;
  private final Class<Collection> collectionDeserializesTo;

  private final RestAssuredConfiguration configuration;

  RestAssuredCollectionApiClient(URI baseUri, OkapiHeaders defaultHeaders,
    Class<Record> recordDeserializesTo,
    Class<Collection> collectionDeserializesTo) {

    configuration = new RestAssuredConfiguration(baseUri, defaultHeaders);

    this.recordDeserializesTo = recordDeserializesTo;
    this.collectionDeserializesTo = collectionDeserializesTo;
  }

  Record createRecord(@NotNull Record user) {
    return attemptToCreateRecord(user)
      .statusCode(HTTP_CREATED)
      .extract().as(recordDeserializesTo);
  }

  ValidatableResponse attemptToCreateRecord(@NotNull Record record) {
    return initialSpecification()
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
    return initialSpecification()
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
    return initialSpecification()
      .when()
      .queryParams("query", cqlQuery, "limit", Integer.MAX_VALUE)
      .get()
      .then();
  }

  ValidatableResponse attemptToUpdateRecord(String id, @NonNull Record record) {
    return initialSpecification()
      .contentType(JSON)
      .when()
      .body(record)
      .put("/{id}", Map.of("id", id))
      .then();
  }

  void deleteRecord(String id) {
    attemptToDeleteRecord(id)
      .statusCode(204);
  }

  ValidatableResponse attemptToDeleteRecord(String id) {
    return initialSpecification()
      .when()
      .delete("/{id}", Map.of("id", id))
      .then();
  }

  void deleteRecords(String cqlQuery) {
    initialSpecification()
      .when()
      .queryParam("query", cqlQuery)
      .delete()
      .then()
      .statusCode(204);
  }

  RequestSpecification initialSpecification() {
    return configuration.initialSpecification();
  }
}
