package org.folio.support;

import java.util.LinkedList;
import java.util.List;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.utils.TenantInit;
import org.folio.support.http.OkapiHeaders;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;

public class VertxModule {
  private final Vertx vertx;
  private final WebClient webClient;

  public VertxModule(Vertx vertx) {
    this.vertx = vertx;
    webClient = WebClient.create(vertx);
  }

  public Future<String> deployModule(int port) {
    final var options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port));

    return vertx.deployVerticle(RestVerticle.class.getName(), options);
  }

  public Future<Void> enableModule(OkapiHeaders headers) {
    final var tenantClient = new TenantClient(headers.getOkapiUrl(),
      headers.getTenantId(), headers.getToken(), webClient);

    TenantAttributes ta = new TenantAttributes();
    ta.setModuleTo("mod-users-1.0.0");
    List<Parameter> parameters = new LinkedList<>();
    parameters.add(new Parameter().withKey("loadReference").withValue("false"));
    parameters.add(new Parameter().withKey("loadSample").withValue("false"));
    ta.setParameters(parameters);

    return TenantInit.init(tenantClient, ta);
  }
}
