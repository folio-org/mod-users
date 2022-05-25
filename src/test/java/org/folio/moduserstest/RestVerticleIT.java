package org.folio.moduserstest;

import static org.folio.moduserstest.RestITSupport.post;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.utils.TenantInit;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestVerticleIT {
  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) {
    Vertx vertx = Vertx.vertx();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();
    RestITSupport.setUp(port);
    TenantClient tenantClient = new TenantClient("http://localhost:" + port,
      "diku", "diku", WebClient.create(vertx));

    DeploymentOptions options = new DeploymentOptions()
        .setConfig(new JsonObject().put("http.port", port));

    //module version number doesn't matter to RAML Module Builder,
    //this is used as a marker for a new activation rather than an upgrade
    vertx.deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(res -> {
      TenantAttributes ta = new TenantAttributes();
      ta.setModuleTo("mod-users-1.0.0");
      List<Parameter> parameters = new LinkedList<>();
      parameters.add(new Parameter().withKey("loadReference").withValue("true"));
      parameters.add(new Parameter().withKey("loadSample").withValue("false"));
      ta.setParameters(parameters);
      TenantInit.init(tenantClient, ta).onComplete(context.asyncAssertSuccess());
    }));
  }

  @Test
  public void testExpiryOK(TestContext context) {
    Map<String,String> headers = new HashMap<>();
    headers.put("Accept", "*/*");
    Future<HttpResponse<Buffer>> future = post("/users/expire/timer", "", headers);
    future.onComplete(context.asyncAssertSuccess(res ->
      context.assertEquals(204, res.statusCode())));
  }

  @Test
  public void testExpiryBadTenant(TestContext context) {
    Map<String,String> headers = new HashMap<>();
    headers.put("Accept", "*/*");
    headers.put("X-Okapi-Tenant", "badTenant");
    Future<HttpResponse<Buffer>> future = post("/users/expire/timer", "", headers);
    future.onComplete(context.asyncAssertSuccess(res ->
      context.assertEquals(500, res.statusCode())));
  }
}
