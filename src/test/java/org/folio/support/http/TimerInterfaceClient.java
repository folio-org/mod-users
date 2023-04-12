package org.folio.support.http;

import io.restassured.response.ValidatableResponse;

public class TimerInterfaceClient {
  private final RestAssuredConfiguration configuration;

  public TimerInterfaceClient(OkapiUrl baseUri, OkapiHeaders defaultHeaders) {
    configuration = new RestAssuredConfiguration(baseUri.asURI(), defaultHeaders);
  }

  public ValidatableResponse attemptToTriggerExpiration(String tenantId) {
    return configuration.initialSpecification()
      .header("X-Okapi-Tenant", tenantId)
      .when()
      .post("/users/expire/timer")
      .then();
  }

  public ValidatableResponse attemptToTriggerUsersOutboxProcess(String tenantId) {
    return configuration.initialSpecification()
      .header("X-Okapi-Tenant", tenantId)
      .when()
      .post("/users/users-outbox/process")
      .then();
  }
}
