package org.folio.rest.utils;

import io.vertx.core.Vertx;

import java.util.Map;

public class OkapiConnectionParams {
  public static final String OKAPI_URL_HEADER = "x-okapi-url";
  public static final String OKAPI_TENANT_HEADER = "x-okapi-tenant";
  public static final String OKAPI_TOKEN_HEADER = "x-okapi-token";
  private String okapiUrl;
  private String tenantId;
  private String token;
  private Map<String, String> headers;
  private final Vertx vertx;
  private int timeout = 2000;

  public OkapiConnectionParams(Map<String, String> okapiHeaders, Vertx vertx) {
    this.okapiUrl = okapiHeaders.getOrDefault(OKAPI_URL_HEADER, "localhost");
    this.tenantId = okapiHeaders.getOrDefault(OKAPI_TENANT_HEADER, "");
    this.token = okapiHeaders.getOrDefault(OKAPI_TOKEN_HEADER, "dummy");
    this.headers = okapiHeaders;
    this.vertx = vertx;
  }

  public OkapiConnectionParams(Vertx vertx) {
    this.vertx = vertx;
  }

  public String getOkapiUrl() {
    return this.okapiUrl;
  }

  public String getTenantId() {
    return this.tenantId;
  }

  public String getToken() {
    return this.token;
  }

  public Map<String, String> getHeaders() {
    return this.headers;
  }

  public Vertx getVertx() {
    return this.vertx;
  }

  public int getTimeout() {
    return this.timeout;
  }

  public void setOkapiUrl(String okapiUrl) {
    this.okapiUrl = okapiUrl;
  }

  public void setTenantId(String tenantId) {
    this.tenantId = tenantId;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public void setHeaders(Map<String, String> headers) {
    this.headers = headers;
  }

  public void setTimeout(int timeout) {
    this.timeout = timeout;
  }
}
