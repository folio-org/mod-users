package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;

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
    return initialSpecification()
      .when()
      .queryParams("limit", Integer.MAX_VALUE)
      .get()
      .then()
      .extract().as(collectionDeserializesTo);
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
      .statusCode(SC_NO_CONTENT);
  }

  void deleteRecord(String id, Map<String, String> customHeaders) {
    attemptToDeleteRecord(id, customHeaders)
      .statusCode(204);
  }

  ValidatableResponse attemptToDeleteRecord(String id) {
    return initialSpecification()
      .when()
      .delete("/{id}", Map.of("id", id))
      .then();
  }

  ValidatableResponse attemptToDeleteRecord(String id, Map<String, String> customHeaders) {
    return initialSpecification()
      .headers(customHeaders)
      .when()
      .delete("/{id}", Map.of("id", id))
      .then();
  }

  void deleteRecords(String cqlQuery) {
    attemptToDeleteRecords(cqlQuery)
      .statusCode(SC_NO_CONTENT);
  }


  ValidatableResponse attemptToDeleteRecords(String cqlQuery) {
    return initialSpecification()
      .when()
      .queryParam("query", cqlQuery)
      .delete()
      .then();
  }

  RequestSpecification initialSpecification() {
    return configuration.initialSpecification();
  }
}
