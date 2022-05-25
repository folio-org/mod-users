package org.folio.support.http;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import java.net.URI;
import java.util.Map;

import org.folio.support.ProxyRelationships;
import org.folio.support.ProxyRelationship;

import io.restassured.builder.RequestSpecBuilder;
import io.restassured.config.LogConfig;
import io.restassured.config.ObjectMapperConfig;
import io.restassured.config.RestAssuredConfig;
import io.restassured.mapper.ObjectMapperType;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import lombok.NonNull;

public class ProxiesClient {
  private final RequestSpecification requestSpecification;
  private final RestAssuredConfig config;

  public ProxiesClient(URI baseUri, OkapiHeaders defaultHeaders) {
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

  public ProxyRelationship createProxyRelationship(
    @NonNull ProxyRelationship proxyRelationship) {

    return attemptToCreateProxyRelationship(proxyRelationship)
      .statusCode(HTTP_CREATED)
      .extract().as(ProxyRelationship.class);
  }

  public ValidatableResponse attemptToCreateProxyRelationship(
    @NonNull ProxyRelationship proxyRelationship) {

    return given()
      .config(config)
      .spec(requestSpecification)
      .contentType(JSON)
      .when()
      .body(proxyRelationship)
      .post("/proxiesfor")
      .then();
  }

  public ProxyRelationships getAllProxyRelationships() {
    return getProxyRelationships("cql.allRecords=1");
  }

  public ProxyRelationships getProxyRelationships(String cqlQuery) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .queryParam("query", cqlQuery)
      .get("/proxiesfor")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(ProxyRelationships.class);
  }

  public void deleteProxyRelationship(String id) {
    attemptToDeleteProxyRelationship(id)
      .statusCode(HTTP_NO_CONTENT);
  }

  public ValidatableResponse attemptToDeleteProxyRelationship(String id) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .delete("/proxiesfor/{id}", Map.of("id", id))
      .then();
  }

  public void deleteAllProxies() {
    final var proxyRelationships = getAllProxyRelationships();

    proxyRelationships.getProxiesFor()
      .forEach(relationship -> deleteProxyRelationship(relationship.getId()));
  }

  public ProxyRelationship getProxyRelationship(String id) {
    return attemptToGetProxyRelationship(id)
      .statusCode(HTTP_OK)
      .extract().as(ProxyRelationship.class);
  }

  public ValidatableResponse attemptToGetProxyRelationship(String id) {
    return given()
      .config(config)
      .spec(requestSpecification)
      .when()
      .get("/proxiesfor/{id}", Map.of("id", id))
      .then();
  }
}
