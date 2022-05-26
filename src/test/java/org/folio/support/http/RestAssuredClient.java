package org.folio.support.http;

import io.restassured.config.RestAssuredConfig;
import io.restassured.specification.RequestSpecification;

public class RestAssuredClient {
  final RequestSpecification requestSpecification;
  final RestAssuredConfig config;

  public RestAssuredClient(RequestSpecification requestSpecification,
    RestAssuredConfig config) {
    this.requestSpecification = requestSpecification;
    this.config = config;
  }
}
