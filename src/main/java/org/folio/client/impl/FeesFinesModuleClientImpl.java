package org.folio.client.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.client.FeesFinesModuleClient;
import org.folio.exceptions.HttpException;
import org.folio.integration.http.ResponseInterpreter;
import org.folio.integration.http.VertxOkapiHttpClient;
import org.folio.util.PercentCodec;
import org.folio.util.StringUtil;

import com.google.common.collect.Maps;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class FeesFinesModuleClientImpl implements FeesFinesModuleClient {

  private static final Logger logger = LogManager.getLogger(FeesFinesModuleClientImpl.class);
  private static final String QUERY = "query";
  private static final String MANUAL_BLOCKS_PATH = "/manualblocks";
  private static final String SLASH = "/";

  private VertxOkapiHttpClient client;

  public FeesFinesModuleClientImpl(VertxOkapiHttpClient client) {
    this.client = client;
  }


  /**
   * Get manual blocks by query
   *
   * @param cqlQuery Query string to filter manual blocks
   * @param okapiHeaders Okapi headers
   * @return CompletableFuture with JsonObject containing manual blocks
   */
  @Override
  public CompletableFuture<JsonObject> getManualBlocksByCQL(String cqlQuery, Map<String, String> okapiHeaders) {
    logger.info("getManualBlocksByCQL::Retrieving manualBlocks by cqlQuery {}", cqlQuery);
    Map<String, String> queryParameters = Maps.newLinkedHashMap();
    queryParameters.put(QUERY, cqlQuery);
    return client.get(MANUAL_BLOCKS_PATH, queryParameters, okapiHeaders)
      .thenApply(response -> {
        logger.info("getManualBlocksByCQL::Successfully retrieved manual blocks by query: {}", cqlQuery);
        return ResponseInterpreter.verifyAndExtractBody(response);
      });
  }

  /**
   * Delete manual block by ID
   *
   * @param manualBlockId ID of the manual block to delete
   * @param okapiHeaders Okapi headers
   * @return CompletableFuture with void result
   */
  @Override
  public CompletableFuture<Void> deleteManualBlockById(String manualBlockId, Map<String, String> okapiHeaders) {
    String path = MANUAL_BLOCKS_PATH + SLASH + manualBlockId;
    return client.delete(path, okapiHeaders)
      .thenApply(response -> {
        if (!response.isDeleted()) {
          throw new CompletionException(new HttpException(response.statusCode,
            String.format("Failed to delete manual block. Status code: %s, body: %s", response.statusCode, response.body)));
        }
        return null;
      });
  }

  /**
   * Delete manual blocks by user ID
   *
   * @param userId User ID to delete manual blocks for
   * @param okapiHeaders Okapi headers
   * @return CompletableFuture with void result
   */
  @Override
  public CompletableFuture<Void> deleteManualBlocksByUserId(String userId, Map<String, String> okapiHeaders) {
    String query = PercentCodec.encode("(userId==" + StringUtil.cqlEncode(userId) + ")").toString();
    return getManualBlocksByCQL(query, okapiHeaders)
      .thenCompose(manualBlocks -> {
        JsonArray blocks = manualBlocks.getJsonArray("manualblocks");
        CompletableFuture<Void> future = CompletableFuture.completedFuture(null);

        for (int i = 0; i < blocks.size(); i++) {
          JsonObject block = blocks.getJsonObject(i);
          String blockId = block.getString("id");
          future = future.thenCompose(v -> deleteManualBlockById(blockId, okapiHeaders));
        }

        return future;
      })
      .exceptionally(throwable -> {
        logger.error("deleteManualBlocksByUserId::Error occurred while retrieving or deleting manual blocks for userId: {}", userId,
          throwable);
        throw new CompletionException(throwable);
      });
  }
}
