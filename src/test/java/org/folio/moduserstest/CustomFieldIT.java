package org.folio.moduserstest;

import static io.vertx.core.json.Json.encode;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import static org.folio.moduserstest.RestITSupport.deleteWithNoContentStatus;
import static org.folio.moduserstest.RestITSupport.getJson;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.postWithOkStatus;
import static org.folio.moduserstest.RestITSupport.put;
import static org.folio.moduserstest.RestITSupport.putWithNoContentStatus;
import static org.folio.util.StringUtil.urlEncode;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import io.restassured.http.Header;
import io.vertx.core.AsyncResult;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class CustomFieldIT {

  private static final Logger log = LoggerFactory.getLogger(CustomFieldIT.class);

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String johnRectangleId = "ae6d1c57-3041-4645-9215-3ca0094b77fc";
  private static final String notExistingCustomField = "notExistingCustomField";

  private static final Header FAKE_TOKEN = new Header(XOkapiHeaders.TOKEN, makeFakeJWT("mockuser8", joeBlockId, "diku"));

  private static final String customFieldsPath = "/custom-fields";

  private static final String customFieldId = "524d3210-9ca2-4f91-87b4-d2227d595aaa";

  private static final String postCustomField = "{\"id\": \"524d3210-9ca2-4f91-87b4-d2227d595aaa\", " +
    "\"name\": \"Department\", " +
    "\"visible\": true, " +
    "\"required\": true, " +
    "\"helpText\": \"Provide a department\", " +
    "\"entityType\": \"user\", " +
    "\"type\": \"TEXTBOX_SHORT\", " +
    "\"order\": 1 }";
  private static final String putCustomField = "{\"id\": \"524d3210-9ca2-4f91-87b4-d2227d595aaa\", " +
    "\"name\": \"Department updated\", " +
    "\"visible\": false, " +
    "\"required\": true, " +
    "\"helpText\": \"Provide a department\", " +
    "\"entityType\": \"user\", " +
    "\"type\": \"TEXTBOX_SHORT\", " +
    "\"order\": 1 }";


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
  public void test1Sequential(TestContext context) {
    Async async = context.async();

    postUser()
      .compose(v -> postCustomField())
      .compose(v -> postUserWithInvalidCustomFieldValueLength(context))
      .compose(v -> postUserWithCustomFields())
      .compose(v -> getUserWithCustomFields(context))
      .compose(v -> deleteUser(context, johnRectangleId))
      .compose(v -> putUserWithNotExistingCustomField(context))
      .compose(v -> postUserWithNotExistingCustomField(context))
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> deleteCustomField(context))
      .onComplete(testResultHandler(context, async));
  }

  @Test
  public void test4CustomFields(TestContext context) {
    Async async = context.async();
    postUser()
      .compose(v -> postCustomField())
      .compose(v -> putCustomField(context))
      .compose(v -> queryCustomField(context)).compose(this::assertCustomFieldValues)
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> deleteCustomField(context))
      .onComplete(testResultHandler(context, async));
  }

  private Handler<AsyncResult<Void>> testResultHandler(TestContext context, Async async) {
    return res -> {
      if (res.succeeded()) {
        async.complete();
      } else {
        res.cause().printStackTrace();
        context.fail(res.cause());
      }
    };
  }

  private Future<Void> postUser() {
    log.info("Creating a new user\n");
    JsonObject user = getUser(joeBlockId, "joeblock");
    return postWithOkStatus(joeBlockId, "/users", user.encode());
  }

  private Future<Void> postUserWithCustomFields() {
    log.info("Creating a new user\n");
    JsonObject user = getUser(johnRectangleId, "johnRectangle");
    addCustomFields(user);
   
    return postWithOkStatus(johnRectangleId, "/users", user.encode());
  }

  private Future<Void> postUserWithInvalidCustomFieldValueLength(TestContext context) {
    log.info("Creating a new user\n");
    JsonObject user = getUser(johnRectangleId, "johnRectangle");
    addInvalidCustomFieldValue(user);
    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      RestITSupport.assertStatus(context, response, 422);
      Errors errors = Json.decodeValue(response.body(), Errors.class);
      context.assertEquals("Maximum length of the value is 150", errors.getErrors().get(0).getMessage());
      return null;
    });
  }

  private JsonObject getUser(String userId, String userName) {
    return new JsonObject()
      .put("id", userId)
      .put("active", true)
      .put("username", userName);
  }

  private static void addCustomFields(JsonObject jsonObject) {
    JsonObject customFields = new JsonObject();
    customFields.put("department_1", "Math");
    jsonObject.put("customFields", customFields);
  }

  private static void addInvalidCustomFieldValue(JsonObject jsonObject) {
    JsonObject customFields = new JsonObject();
    customFields.put("department_1", RandomStringUtils.randomAlphanumeric(151));
    jsonObject.put("customFields", customFields);
  }

  private Future<Void> getUserWithCustomFields(TestContext context) {
    log.info("Retrieving a user with custom fields\n");

    Future<JsonObject> future = getJson(context, "/users/" + johnRectangleId);

    return future.map(user -> {
      JsonObject customFields = user.getJsonObject("customFields");

      if (customFields == null || !customFields.encode().equals("{\"department_1\":\"Math\"}")) {
        fail("Bad value for customFields. " + encode(customFields));
      }

      return null;
    });
  }

  private Future<Void> putUserWithNotExistingCustomField(TestContext context) {
    log.info("Changing a user with not existing custom field");

    JsonObject user = new JsonObject()
      .put("username", "johnrectangle")
      .put("id", johnRectangleId)
      .put("active", true)
      .put("personal", new JsonObject()
        .put("lastName", "Rectangle")
        .put("firstName", "John")
      )
      .put("customFields", new JsonObject()
        .put(notExistingCustomField, "abc")
      );

    Future<HttpResponse<Buffer>> future = put("/users/" + johnRectangleId, encode(user));

    return future.map(response -> {
      RestITSupport.assertStatus(context, response, 422);

      Errors errors = Json.decodeValue(response.body(), Errors.class);

      Parameter errorParam = errors.getErrors().get(0).getParameters().get(0);

      context.assertEquals("customFields", errorParam.getKey());
      context.assertEquals(notExistingCustomField, errorParam.getValue());

      return null;
    });
  }

  private Future<Void> postUserWithNotExistingCustomField(TestContext context) {
    log.info("Attempting to create a user with not existing custom field");

    JsonObject user = new JsonObject()
      .put("username", "johnrectangle")
      .put("id", johnRectangleId)
      .put("active", true)
      .put("personal", new JsonObject()
        .put("lastName", "Rectangle")
        .put("firstName", "John")
      )
      .put("customFields", new JsonObject()
        .put(notExistingCustomField, "abc")
      );

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      RestITSupport.assertStatus(context, response, 422);

      Errors errors = Json.decodeValue(response.body(), Errors.class);
      Parameter errorParam = errors.getErrors().get(0).getParameters().get(0);

      context.assertEquals("customFields", errorParam.getKey());
      context.assertEquals(notExistingCustomField, errorParam.getValue());

      return null;
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

    return getJson(context, requestUrl);
  }

  private Future<Void> postCustomField() {
    log.info("Creating a new custom field definition\n");
    return postWithOkStatus(joeBlockId, customFieldsPath, postCustomField, FAKE_TOKEN);
  }

  private Future<Void> putCustomField(TestContext context) {
    log.info("Update custom field definition\n");
    return putWithNoContentStatus(context, joeBlockId, customFieldsPath + "/" + customFieldId, putCustomField, FAKE_TOKEN);
  }

  private Future<Void> deleteUser(TestContext context, String userId) {
    log.info("Deleting existing user\n");
    return deleteWithNoContentStatus(context, "/users/" + userId);
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
