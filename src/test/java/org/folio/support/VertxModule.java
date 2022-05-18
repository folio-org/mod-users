package org.folio.support;

import org.folio.rest.RestVerticle;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class VertxModule {
  private final Vertx vertx;

  public VertxModule(Vertx vertx) {
    this.vertx = vertx;
  }

  public Future<String> deployModule(int port) {
    final var options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port));

    return vertx.deployVerticle(RestVerticle.class.getName(), options);
  }
}
