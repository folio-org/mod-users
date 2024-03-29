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
      .put("/{id}", Map.of("id", config.getId()))
      .then();
  }

  public String getConfigurationId() {
    return client.initialSpecification()
      .when()
      .get()
      .then()
      .log().all()
      .extract()
      .jsonPath()
      .getString("id");
  }
}
