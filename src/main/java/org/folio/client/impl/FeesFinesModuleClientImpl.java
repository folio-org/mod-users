package org.folio.client.impl;

import java.util.Map;
import java.util.concurrent.CompletionException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.client.FeesFinesModuleClient;
import org.folio.exceptions.HttpException;
import org.folio.integration.http.ResponseInterpreter;
import org.folio.integration.http.VertxOkapiHttpClient;
import org.folio.util.StringUtil;

import com.google.common.collect.Maps;

import io.vertx.core.Future;
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
   * @return Future with JsonObject containing manual blocks
   */
  @Override
  public Future<JsonObject> getManualBlocksByCQL(String cqlQuery, Map<String, String> okapiHeaders) {
    logger.info("getManualBlocksByCQL::Retrieving manualBlocks by cqlQuery {}", cqlQuery);
    Map<String, String> queryParameters = Maps.newLinkedHashMap();
    queryParameters.put(QUERY, cqlQuery);
    return client.get(MANUAL_BLOCKS_PATH, queryParameters, okapiHeaders)
      .compose(response -> {
        logger.info("getManualBlocksByCQL::Successfully retrieved manual blocks by query: {}", cqlQuery);
        return ResponseInterpreter.verifyAndExtractBody(response);
      }).recover(throwable -> {
        logger.error("getManualBlocksByCQL::Error occurred while retrieving manual blocks by query: {}", cqlQuery, throwable);
        return Future.failedFuture(throwable);
      });
  }

  /**
   * Delete manual block by ID
   *
   * @param manualBlockId ID of the manual block to delete
   * @param okapiHeaders Okapi headers
   * @return Future with void result
   */
  @Override
  public Future<Void> deleteManualBlockById(String manualBlockId, Map<String, String> okapiHeaders) {
    String path = MANUAL_BLOCKS_PATH + SLASH + manualBlockId;
    return client.delete(path, okapiHeaders)
      .compose(response -> {
        if (!response.isDeleted()) {
          throw new CompletionException(new HttpException(response.statusCode,
            String.format("Failed to delete manual block. Status code: %s, body: %s", response.statusCode, response.body)));
        }
        return Future.<Void>succeededFuture();
      }).recover(throwable -> {
        logger.error("deleteManualBlockById::Error occurred while deleting manual block with id: {}", manualBlockId, throwable);
        return Future.failedFuture(throwable);
      });
  }

  /**
   * Delete manual blocks by user ID
   *
   * @param userId User ID to delete manual blocks for
   * @param okapiHeaders Okapi headers
   * @return Future with void result
   */
  @Override
  public Future<Void> deleteManualBlocksByUserId(String userId, Map<String, String> okapiHeaders) {
    String query = String.format("(userId==%s)", StringUtil.cqlEncode(userId));
    return getManualBlocksByCQL(query, okapiHeaders)
      .compose(manualBlocks -> {
        JsonArray blocks = manualBlocks.getJsonArray("manualblocks");
        Future<Void> future = Future.succeededFuture(null);

        for (int i = 0; i < blocks.size(); i++) {
          JsonObject block = blocks.getJsonObject(i);
          String blockId = block.getString("id");
          future = future.compose(v -> deleteManualBlockById(blockId, okapiHeaders));
        }

        return future;
      })
      .recover(throwable -> {
        logger.error("deleteManualBlocksByUserId::Error occurred while retrieving or deleting manual blocks for userId: {}", userId,
          throwable);
        return Future.failedFuture(throwable);
      });
  }
}
