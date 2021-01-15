package org.folio.moduserstest;

import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.UserExpiryImpl;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.rules.Timeout;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class UserExpiryImplTest {

  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) throws SQLException {
    vertx = Vertx.vertx();
    try {
      PostgresClient.setIsEmbedded(true);
      PostgresClient.getInstance(vertx).startEmbeddedPostgres();
    } catch (Exception e) {
      e.printStackTrace();
      context.fail(e);
      return;
    }

    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    RestITSupport.setUp(port);
    TenantClient tenantClient = new TenantClient("http://localhost:" + Integer.toString(port), "diku", "diku");
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port));

    vertx.deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(res -> {
      try {
        TenantAttributes ta = new TenantAttributes();
        ta.setModuleTo("mod-users-1.0.0");
        List<Parameter> parameters = new LinkedList<>();
        parameters.add(new Parameter().withKey("loadReference").withValue("true"));
        parameters.add(new Parameter().withKey("loadSample").withValue("false"));
        ta.setParameters(parameters);
        tenantClient.postTenant(ta, res2 -> {
          context.assertEquals(201, res2.result().statusCode(), "postTenant: " + res2.result().statusMessage());
          async.complete();
        });
      } catch (Exception e) {
        context.fail(e);
      }
    }));
  }

  @AfterClass
  public static void teardown(TestContext context) {
    Async async = context.async();
    vertx.close(context.asyncAssertSuccess(res -> {
      PostgresClient.stopEmbeddedPostgres();
      async.complete();
    }));
  }

  @Test
  public void postException(TestContext context) {
    context.verify(v -> {
      new UserExpiryImpl().run(vertx, vertx.getOrCreateContext());
    });
  }

}
