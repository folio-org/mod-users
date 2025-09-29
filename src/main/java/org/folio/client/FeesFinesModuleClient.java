package org.folio.client;

import java.util.Map;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public interface FeesFinesModuleClient {
  Future<JsonObject> getManualBlocksByCQL(String cqlQuery, Map<String, String> okapiHeaders);

  Future<Void> deleteManualBlockById(String manualBlockId, Map<String, String> okapiHeaders);

  Future<Void> deleteManualBlocksByUserId(String userId, Map<String, String> okapiHeaders);
}
