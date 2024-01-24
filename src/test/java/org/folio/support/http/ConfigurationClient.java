package org.folio.support.http;

import io.restassured.response.ValidatableResponse;
import org.folio.rest.jaxrs.model.Config;

import java.util.Map;

public class ConfigurationClient {

  private final RestAssuredConfiguration client;

  public ConfigurationClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    this.client = new RestAssuredConfiguration(okapiUrl.asURI("/users/configurations/entry"), defaultHeaders);
  }

  public ValidatableResponse updateConfiguration(Config config) {
    return client.initialSpecification()
      .contentType("application/json")
      .when()
      .body(config)
      .put("/{configName}", Map.of("configName", config.getConfigName()))
      .then();
  }

  public ValidatableResponse getConfiguration(String id) {
    return client.initialSpecification()
      .when()
      .get("/{id}", Map.of("id", id))
      .then();
  }

}
