package org.folio.rest.utils;

import java.util.Map;

import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.Context;

/**
 * Helper for PostgresClient access.
 */
public final class PostgresClientUtil {
  private PostgresClientUtil() {
    throw new UnsupportedOperationException("Cannot instantiate utility class");
  }

  /**
   * Return a PostgresClient. Invokes setIdField("id") on it.
   * @param vertxContext  Where to get a Vertx from.
   * @param okapiHeaders  Where to get the tenantId from.
   * @return the PostgresClient instance for the vertx and the tenantId
   */
  public static PostgresClient getInstance(Context vertxContext, Map<String, String> okapiHeaders) {
    PostgresClient postgresClient = PostgresClient.getInstance(
        vertxContext.owner(), TenantTool.tenantId(okapiHeaders));
    return postgresClient;
  }
}
