package org.folio.support.http;

import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;

import java.util.Map;

import static io.restassured.http.ContentType.JSON;
import static java.net.HttpURLConnection.HTTP_CREATED;

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

  public void saveUserTenant(UserTenant userTenant) {
    configuration.initialSpecification()
      .contentType(JSON)
      .when()
      .body(userTenant)
      .post()
      .then()
      .statusCode(HTTP_CREATED);
  }
}
