package org.folio.moduserstest;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
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

  private static final String FAKE_TOKEN = makeFakeJWT("bubba", UUID.randomUUID().toString(), "diku");

  @BeforeClass
  public static void setup(TestContext context) {
    RestITSupport.setUp();
    Async async = context.async();
    TenantClient tenantClient = new TenantClient(RestITSupport.HTTP_LOCALHOST + RestITSupport.port(), "diku", FAKE_TOKEN);
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", RestITSupport.port()))
      .setWorker(true);
    RestITSupport.vertx().deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(res -> {
      // remove existing schema from previous tests
      tenantClient.deleteTenant(delete -> {
        switch (delete.statusCode()) {
          case 204: break;  // existing schema has been deleted
          case 400: break;  // schema does not exist
          default:
            RestITSupport.fail(context, "deleteTenant", delete);
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
            if (post.statusCode() != 201) {
              RestITSupport.fail(context, "postTenant", post);
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

  private static String makeFakeJWT(String username, String id, String tenant) {
    JsonObject header = new JsonObject()
      .put("alg", "HS512");
    JsonObject payload = new JsonObject()
      .put("sub", username)
      .put("user_id", id)
      .put("tenant", tenant);
    return String.format("%s.%s.%s",
      Base64.getEncoder().encodeToString(header.encode()
        .getBytes(StandardCharsets.UTF_8)),
      Base64.getEncoder().encodeToString(payload.encode()
        .getBytes(StandardCharsets.UTF_8)),
      Base64.getEncoder().encodeToString((header.encode() + payload.encode())
        .getBytes(StandardCharsets.UTF_8)));
  }
}
