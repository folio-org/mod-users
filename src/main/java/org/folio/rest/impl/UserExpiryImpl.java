package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import static org.folio.rest.RestVerticle.MODULE_SPECIFIC_ARGS;
import org.folio.rest.resource.interfaces.PeriodicAPI;
import org.folio.rest.utils.ExpirationTool;

/**
 *
 * @author kurt
 */
public class UserExpiryImpl implements PeriodicAPI {


  @Override
  public long runEvery() {
    String intervalString = MODULE_SPECIFIC_ARGS.getOrDefault("expire.interval",
            "30000");
    return Long.parseLong(intervalString);
  }

  @Override
  public void run(Vertx vertx, Context context) {
    context.runOnContext(v -> {
      ExpirationTool.doExpiration(vertx, context);
    });
  }

}
