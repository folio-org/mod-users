package org.folio.integration.http;

import static org.apache.commons.lang3.StringUtils.isBlank;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.exceptions.HttpException;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ResponseInterpreter {
  private static final Logger log = LogManager.getLogger();
  private ResponseInterpreter() {}

  public static Future<JsonObject> verifyAndExtractBody(Response response) {
    log.debug("verifyAndExtractBody:: statusCode: {}, body: {}", response.statusCode, response.body);
    if (!response.isSuccess()) {
      throw new HttpException(response.statusCode, response.body);
    }

    // Parsing an empty body to JSON causes an exception
    if (isBlank(response.body)) {
      log.info("verifyAndExtractBody:: response is empty");
      return null;
    }
    return Future.succeededFuture(new JsonObject(response.body));
  }
}
