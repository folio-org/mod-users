package org.folio.client;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.json.JsonObject;

public interface FeesFinesModuleClient {
  CompletableFuture<JsonObject> getManualBlocksByCQL(String cqlQuery, Map<String, String> okapiHeaders);

  CompletableFuture<Void> deleteManualBlockById(String manualBlockId, Map<String, String> okapiHeaders);

  CompletableFuture<Void> deleteManualBlocksByUserId(String userId, Map<String, String> okapiHeaders);
}
