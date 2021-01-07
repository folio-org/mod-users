package org.folio.moduserstest;

import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.UserExpiryImpl;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.test.util.TokenTestUtil;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class UserExpiryImplTest {

  private static final String FAKE_TOKEN = TokenTestUtil.generateToken("bubba", UUID.randomUUID().toString());

  @BeforeClass
  public static void setup(TestContext context) {
    RestITSupport.setUp();
    Async async = context.async();
    TenantClient tenantClient = new TenantClient(RestITSupport.HTTP_LOCALHOST + RestITSupport.port(), "diku", FAKE_TOKEN);
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", String.valueOf(RestITSupport.port())));
    RestITSupport.vertx().deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(res -> {
      // remove existing schema from previous tests
      tenantClient.deleteTenantByOperationId("diku", delete -> {
        switch (delete.result().statusCode()) {
          case 204: break;  // existing schema has been deleted
          case 400: break;  // schema does not exist
          default:
            RestITSupport.fail(context, "deleteTenant", delete.result());
            return;
        }
        try {
          TenantAttributes ta = new TenantAttributes();
          ta.setModuleTo("mod-users-1.0.0");
          List<Parameter> parameters = new LinkedList<>();
          parameters.add(new Parameter().withKey("loadReference").withValue("true"));
          parameters.add(new Parameter().withKey("loadSample").withValue("false"));
          ta.setParameters(parameters);
          tenantClient.postTenant(ta, post -> {
            if (post.result().statusCode() != 201) {
              RestITSupport.fail(context, "postTenant", post.result());
            }
            async.complete();
          });
        } catch (Exception e) {
          context.fail(e);
        }
      });
    }));
  }

  @AfterClass
  public static void tearDown() {
    CompletableFuture<Void> future = new CompletableFuture<>();
    RestITSupport.vertx().close(res -> {
      PostgresClient.stopEmbeddedPostgres();
      future.complete(null);
    });
    future.join();
  }

  @Test
  public void postException(TestContext context) {
    Vertx vertx = RestITSupport.vertx();
    context.verify(v -> {
      new UserExpiryImpl().run(vertx, vertx.getOrCreateContext());
    });
  }

}
