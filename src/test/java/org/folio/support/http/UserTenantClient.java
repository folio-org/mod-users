package org.folio.support.http;

import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;

import java.util.Map;

public class UserTenantClient {
  private final RestAssuredConfiguration configuration;

  public UserTenantClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    configuration = new RestAssuredConfiguration(okapiUrl.asURI("/user-tenants"), defaultHeaders);
  }

  public UserTenantCollection getUserTenants(Map<String, String> queryParams) {
    return configuration.initialSpecification()
      .when()
      .queryParams(queryParams)
      .get()
      .then()
      .extract().as(UserTenantCollection.class);
  }

  public UserTenantCollection getAllUsersTenants() {
    return configuration.initialSpecification()
      .when()
      .queryParams("limit", Integer.MAX_VALUE)
      .get()
      .then()
      .extract().as(UserTenantCollection.class);
  }

  public int attemptToSaveUserTenant(UserTenant userTenant) {
    return configuration.initialSpecification()
      .contentType("application/json")
      .when()
      .body(userTenant)
      .post()
      .then()
      .extract().statusCode();
  }

  public void deleteUserTenant(String id) {
   configuration.initialSpecification()
     .when()
     .delete("/{id}", Map.of("id", id))
     .then();
  }

  public void deleteAllUserTenants() {
    final var usersTenants = getAllUsersTenants();
    usersTenants.getUserTenants().forEach(addressType -> deleteUserTenant(addressType.getId()));
  }
}
