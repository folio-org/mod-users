package org.folio.moduserstest;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import static org.folio.moduserstest.RestITSupport.deleteWithNoContentStatus;
import static org.folio.moduserstest.RestITSupport.getByQuery;
import static org.folio.moduserstest.RestITSupport.postWithOkStatus;
import static org.folio.moduserstest.RestITSupport.putWithNoContentStatus;
import static org.folio.util.StringUtil.urlEncode;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;

@RunWith(VertxUnitRunner.class)
public class CustomFieldIT {

  private static final Logger log = LoggerFactory.getLogger(CustomFieldIT.class);

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";

  private static final String customFieldsPath = "/custom-fields";

  private static final String customFieldId = "524d3210-9ca2-4f91-87b4-d2227d595aaa";

  private static final String postCustomField = "{\"id\": \"524d3210-9ca2-4f91-87b4-d2227d595aaa\", " +
    "\"name\": \"Department\", " +
    "\"visible\": true, " +
    "\"required\": true, " +
    "\"helpText\": \"Provide a department\", " +
    "\"entityType\": \"user\", " +
    "\"type\": \"TEXTBOX_SHORT\", " +
    "\"order\": 1, " +
    "\"textField\": { \"maxSize\": 150 }}";
  private static final String putCustomField = "{\"id\": \"524d3210-9ca2-4f91-87b4-d2227d595aaa\", " +
    "\"name\": \"Department updated\", " +
    "\"visible\": false, " +
    "\"required\": true, " +
    "\"helpText\": \"Provide a department\", " +
    "\"entityType\": \"user\", " +
    "\"type\": \"TEXTBOX_SHORT\", " +
    "\"order\": 1, " +
    "\"textField\": {   \"maxSize\": 250 }}";


  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) {
    RestITSupport.setUp();

    Async async = context.async();
    TenantClient tenantClient = new TenantClient(RestITSupport.HTTP_LOCALHOST + RestITSupport.port(), "diku", "diku");
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
  public void test4CustomFields(TestContext context) {

    Async async = context.async();
    postCustomField()
      .compose(v -> putCustomField(context))
      .compose(v -> queryCustomField(context)).compose(this::assertCustomFieldValues)
      .compose(v -> deleteCustomField(context))
      .setHandler(res -> {
        if (res.succeeded()) {
          async.complete();
        } else {
          res.cause().printStackTrace();
          context.fail(res.cause());
        }
      });
  }

  private Future<Void> assertCustomFieldValues(JsonObject result) {
    Promise<Void> promise = Promise.promise();
    int totalRecords = result.getInteger("totalRecords");
    if (totalRecords != 1) {
      promise.fail("Expected 1 record, got " + totalRecords);
    }
    JsonArray customFields = result.getJsonArray("customFields");
    JsonObject customField = customFields.getJsonObject(0);
    assertThat(customField.getString("entityType"), is("user"));

    promise.complete();
    return promise.future();
  }

  private Future<Void> deleteCustomField(TestContext context) {
    log.info("Deleting existing custom field\n");
    return deleteWithNoContentStatus(context, customFieldsPath + "/" + customFieldId);
  }

  private Future<JsonObject> queryCustomField(TestContext context) {
    String requestUrl = customFieldsPath + "?query=" + urlEncode("entityType==user");
    log.info("Getting custom field via CQL, by entityType\n");

    return getByQuery(context, requestUrl);
  }

  private Future<Void> postCustomField() {
    log.info("Creating a new custom field definition\n");
    return postWithOkStatus(joeBlockId, customFieldsPath, postCustomField);
  }

  private Future<Void> putCustomField(TestContext context) {
    log.info("Update custom field definition\n");
    return putWithNoContentStatus(context, joeBlockId, customFieldsPath + "/" + customFieldId, putCustomField);
  }
}
