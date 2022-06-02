package org.folio.support.http;

import org.folio.support.ProxyRelationship;
import org.folio.support.ProxyRelationships;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;

public class ProxiesClient {
  private final RestAssuredCollectionApiClient<ProxyRelationship, ProxyRelationships> client;

  public ProxiesClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/proxiesfor"),
      defaultHeaders, ProxyRelationship.class, ProxyRelationships.class);
  }

  public ProxyRelationship createProxyRelationship(
    @NonNull ProxyRelationship proxyRelationship) {

    return client.createRecord(proxyRelationship);
  }

  public ValidatableResponse attemptToCreateProxyRelationship(
    @NonNull ProxyRelationship proxyRelationship) {

    return client.attemptToCreateRecord(proxyRelationship);
  }

  public ProxyRelationships getAllProxyRelationships() {
    return client.getAllRecords();
  }

  public ProxyRelationships getProxyRelationships(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public void deleteProxyRelationship(String id) {
    client.attemptToDeleteRecord(id);
  }

  public ValidatableResponse attemptToDeleteProxyRelationship(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteAllProxies() {
    final var proxyRelationships = getAllProxyRelationships();

    proxyRelationships.getProxiesFor()
      .forEach(relationship -> deleteProxyRelationship(relationship.getId()));
  }

  public ProxyRelationship getProxyRelationship(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptToGetProxyRelationship(String id) {
    return client.attemptToGetRecord(id);
  }

  public ValidatableResponse attemptToUpdateProxyRelationship(
    ProxyRelationship proxyRelationship) {

    return client.attemptToUpdateRecord(proxyRelationship.getId(),
      proxyRelationship);
  }
}
