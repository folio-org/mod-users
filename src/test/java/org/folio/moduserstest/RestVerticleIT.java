package org.folio.moduserstest;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertThat;

import static org.folio.moduserstest.RestITSupport.deleteWithNoContentStatus;
import static org.folio.util.StringUtil.urlEncode;

import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import org.joda.time.DateTime;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.tools.utils.VertxUtils;
import org.folio.util.StringUtil;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestVerticleIT {

  private static final Logger log = LoggerFactory.getLogger(RestVerticleIT.class);

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String bobCircleId = "54afd8b8-fb3b-4de8-9b7c-299904887f7d";
  private static final String jackTriangleId = "e133841d-b645-4488-9e52-9762d560b617";
  private static final String johnRectangleId = "ae6d1c57-3041-4645-9215-3ca0094b77fc";
  private static final String annaRhombusId = "e8090974-8876-4411-befa-8ddcffad0b35";
  private static final String user777777Id = "72bd29f7-bf29-48bb-8259-d5ce78378a56";
  private static final String userIdWithWhitespace = "56bd29f7-bf29-48bb-8259-d5ce76378a42";
  private static final String notExistingCustomField = "notExistingCustomField";

  private JsonObject testAddress = new JsonObject().put("addressType", "school")
    .put("desc", "Patron's School")
    .put("id", UUID.randomUUID().toString());

  private JsonObject testGroup = new JsonObject().put("group", "dropouts")
    .put("desc", "Freaks and Geeks")
    .put("id", UUID.randomUUID().toString());

  private JsonObject testProxyFor = new JsonObject()
    .put("userId", UUID.randomUUID().toString())
    .put("proxyUserId", UUID.randomUUID().toString())
    .put("status", "Active")
    .put("expirationDate",
      new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'").format(new Date()))
    .put("requestForSponsor", "Yes")
    .put("notificationsTo", "Proxy")
    .put("accrueTo", "Sponsor");

  private static Vertx vertx;
  private static WebClient client;
  static int port;


  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) {
    vertx = VertxUtils.getVertxWithExceptionHandler();

    client = WebClient.create(vertx);

    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient(RestITSupport.HTTP_LOCALHOST + port, "diku", "diku");
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port))
      .setWorker(true);

    vertx.deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(res -> {
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
    vertx.close(res -> {
      PostgresClient.stopEmbeddedPostgres();
      future.complete(null);
    });
    future.join();
  }

  private Future<Void> getEmptyUsers(TestContext context) {
    log.info("Getting an empty user set\n");
    Promise<Void> promise = Promise.promise();
    //HttpClient client = vertx.createHttpClient();

    client.get(port, "localhost", "/users")
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .send(ar -> {
        if (ar.succeeded()) {
          HttpResponse<Buffer> response = ar.result();
          RestITSupport.assertStatus(context, response, 200);

          JsonObject userCollectionObject = response.bodyAsJsonObject();;
          if (userCollectionObject.getJsonArray("users").size() == 0
            && userCollectionObject.getInteger("totalRecords") == 00) {
            promise.complete();
          } else {
            promise.fail("Invalid return JSON: " + response.bodyAsString());
          }
        } else {
          promise.fail(ar.cause());
        }
      });

    /*client.get(port, "localhost", "/users", res -> {
      assertStatus(context, res, 200);
      res.bodyHandler(buf -> {
        JsonObject userCollectionObject = buf.toJsonObject();
        if (userCollectionObject.getJsonArray("users").size() == 0
          && userCollectionObject.getInteger("totalRecords") == 00) {
          promise.complete();
        } else {
          promise.fail("Invalid return JSON: " + buf.toString());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();*/
    return promise.future();
  }

  private static void addTags(JsonObject u) {
    JsonArray tagList = new JsonArray();
    tagList.add("foo-tag");
    tagList.add("bar-tag");
    JsonObject tagobj = new JsonObject();
    tagobj.put("tagList", tagList);
    u.put("tags", tagobj);
  }

  private static void addCustomFields(JsonObject u) {
    JsonObject customFields = new JsonObject();
    customFields.put("department_1", "Math");
    u.put("customFields", customFields);
  }

  private Future<Void> postUser(TestContext context, boolean withUserName) {
    log.info("Creating a new user\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("id", joeBlockId)
      .put("active", true);
    if (withUserName) {
      userObject.put("username", "joeblock");
    }
    addTags(userObject);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if (res.statusCode() >= 200 && res.statusCode() < 300) {
        promise.complete();
      } else {
        promise.fail("Got status code: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> postUserWithCustomFields(TestContext context) {
    log.info("Creating a new user\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("id", johnRectangleId)
      .put("active", true)
      .put("username", "johnRectangle");
    addCustomFields(userObject);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if (res.statusCode() >= 200 && res.statusCode() < 300) {
        promise.complete();
      } else {
        promise.fail("Got status code: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> deleteNonExistingUser(TestContext context) {
    log.info("Deleting non-existing user\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.delete(port, "localhost", "/users/85936906-4737-4da7-b0fb-e8da080b97d8", res -> {
      RestITSupport.assertStatus(context, res, 404);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("accept", "*/*")
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> deleteUser(TestContext context, String userId) {
    log.info("Deleting existing user\n");
    return deleteWithNoContentStatus(context, "/users/" + userId);
  }

  private Future<Void> postUserWithNumericName(TestContext context) {
    log.info("Creating a user with a numeric name\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> getUser(TestContext context) {
    log.info("Retrieving a user\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users/" + joeBlockId, res -> {
      if (res.statusCode() == 200) {
        res.bodyHandler(buf -> {
          JsonObject userObject = buf.toJsonObject();
          if (userObject.getString("username").equals("joeblock")) {
            JsonObject tags = userObject.getJsonObject("tags");

            if (tags == null || !tags.encode().equals("{\"tagList\":[\"foo-tag\",\"bar-tag\"]}")) {
              promise.fail("Bad value for tag list. " + buf.toString());
            }
            else {
              DateFormat gmtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'");
              Date createdDate = null;
              try {
                //createdDate = DatatypeConverter.parseDateTime(userObject.getString("createdDate")).getTime();
                createdDate = new DateTime(userObject.getString("createdDate")).toDate();
              } catch (Exception e) {
                promise.fail(e);
                return;
              }
              Date now = new Date();
              if (createdDate.after(now)) {
                promise.fail(new AssertionError("Bad value for createdDate"));
                return;
              }
              promise.complete();
            }
          } else {
            promise.fail("Unable to read proper data from JSON return value: " + buf.toString());
          }
        });
      } else {
        promise.fail("Bad response: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> getUserWithCustomFields(TestContext context) {
    log.info("Retrieving a user with custom fields\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users/" + johnRectangleId, res -> {
      if (res.statusCode() == 200) {
        res.bodyHandler(buf -> {
          JsonObject userObject = buf.toJsonObject();
          JsonObject customFields = userObject.getJsonObject("customFields");
          if (customFields == null || !customFields.encode().equals("{\"department_1\":\"Math\"}")) {
            promise.fail("Bad value for customFields. " + buf.toString());
          }else{
            promise.complete();
          }
        });
      } else {
        promise.fail("Bad response: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> getUserByCQL(TestContext context) {
    log.info("Getting user via CQL, by username\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + StringUtil.urlEncode("username==joeblock"), res -> {
        RestITSupport.assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            if (totalRecords != 1) {
              promise.fail("Expected 1 record, got " + totalRecords);
              return;
            }
            JsonArray userList = resultObject.getJsonArray("users");
            JsonObject userObject = userList.getJsonObject(0);
            if (userObject.getString("username").equals("joeblock")) {
              promise.complete();
            } else {
              promise.fail("Unable to read proper data from JSON return value: " + buf.toString());
            }
          } catch (Exception e) {
            promise.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> getUserByCqlById(TestContext context) {
    log.info("Getting user via CQL, by user id\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + urlEncode("(id==" + joeBlockId + ")"), res -> {
        RestITSupport.assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            assertThat(totalRecords, is(1));
            JsonArray userList = resultObject.getJsonArray("users");
            JsonObject userObject = userList.getJsonObject(0);
            assertThat("username of " + buf, userObject.getString("username"), is("joeblock"));
            promise.complete();
          } catch (Exception e) {
            promise.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> getUserByInvalidCQL(TestContext context) {
    log.info("Getting user via invalid CQL\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    try {
      // empty CQL query triggers parse exception
      client.get(port, "localhost", "/users?query=", res -> {
        RestITSupport.assertStatus(context, res, 400);
        promise.complete();
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> postAnotherUser(TestContext context) {
    log.info("Creating another user\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "bobcircle")
      .put("id", bobCircleId)
      .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res,  201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> getUsersByCQL(TestContext context, String cql, String... expectedUsernames) {
    log.info("Query users via CQL\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + urlEncode(cql), res -> {
        RestITSupport.assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            JsonArray userList = resultObject.getJsonArray("users");
            if (userList.size() != totalRecords) {
              promise.fail("totalRecords=" + totalRecords + " mismatch users list: " + userList.encodePrettily());
              return;
            }
            List<String> usernames = new ArrayList<>();
            for (int i = 0; i < userList.size(); i++) {
              usernames.add(userList.getJsonObject(i).getString("username"));
            }
            assertThat(usernames, containsInAnyOrder(expectedUsernames));
            promise.complete();
          } catch (Exception e) {
            promise.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> putUserGood(TestContext context, String id, boolean withUserName) {
    log.info("Making a valid user modification\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("id", id)
      .put("active", false);
    if (withUserName) {
      userObject.put("username", "bobcircle");
    }
    addCustomFields(userObject);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + id, res -> {
      RestITSupport.assertStatus(context, res, 204);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> getGoodUser(TestContext context) {
    log.info("Getting the modified user\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users/" + bobCircleId, res -> {
      RestITSupport.assertStatus(context, res, 200);
      res.bodyHandler(buf -> {
        JsonObject userObject = buf.toJsonObject();
        if (userObject.getString("username").equals("bobcircle")) {
          Date createdDate;
          Date updatedDate;
          try {
            createdDate = new DateTime(userObject.getString("createdDate")).toDate();
            updatedDate = new DateTime(userObject.getString("updatedDate")).toDate();
          } catch (Exception e) {
            promise.fail(e);
            return;
          }
          Date now = new Date();
          if (createdDate.after(now) || updatedDate.after(now) || createdDate.after(updatedDate)) {
            promise.fail(new AssertionError("Bad value for createdDate and/or updatedDate"));
            return;
          }
          promise.complete();
        } else {
          promise.fail("Unable to read proper data from JSON return value: " + buf.toString());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> putUserBadUsername(TestContext context) {
    log.info("Trying to assign an invalid username \n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", bobCircleId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + bobCircleId, res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserWithoutIdInMetadata(TestContext context) {
    log.info("Changing a user without id in metadata\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
        .put("username", "bobcircle")
        .put("id", bobCircleId)
        .put("active", false)
        // metadata with createdDate but without createdByUserId
        // https://issues.folio.org/browse/RMB-459
        // https://issues.folio.org/browse/UIU-1069
        .put("metadata", new JsonObject().put("createdDate", "2000-12-31T01:02:03"));
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + bobCircleId, res -> {
      RestITSupport.assertStatus(context, res, 204);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(context::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserWithNotExistingCustomField(TestContext context) {
    log.info("Changing a user with not existing custom field");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
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
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + johnRectangleId, res -> {
      RestITSupport.assertStatus(context, res, 422);
      res.bodyHandler(err -> {
        Errors errors = Json.decodeValue(err, Errors.class);
        Parameter errorParam = errors.getErrors().get(0).getParameters().get(0);
        context.assertEquals("customFields", errorParam.getKey());
        context.assertEquals(notExistingCustomField, errorParam.getValue());
        promise.complete();
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserBadId(TestContext context) {
    log.info("Trying to assign an invalid id \n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "bobcircle")
      .put("id", joeBlockId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + joeBlockId, res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserNotMatchingId(TestContext context) {
    log.info("Trying to Update user id \n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", bobCircleId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + joeBlockId, res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserDuplicatedAddressType(TestContext context) {
    log.info("Attempting to update a user with two of the same address types\n");
    Promise<Void> promise = Promise.promise();
    String addressTypeId = "4716a236-22eb-472a-9f33-d3456c9cc9d5";
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", joeBlockId)
      .put("active", false)
      .put("personal", new JsonObject()
        .put("lastName", "Joe")
        .put("firstName", "Block")
        .put("addresses", new JsonArray()
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "123 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", addressTypeId)
          )
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "234 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", addressTypeId)
          )
        )
      );
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + joeBlockId, res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> putUserInvalidAddressType(TestContext context) {
    log.info("Attempting to update a user with invalid address types\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", joeBlockId)
      .put("active", false)
      .put("personal", new JsonObject()
        .put("lastName", "Joe")
        .put("firstName", "Block")
        .put("addresses", new JsonArray()
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "123 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", UUID.randomUUID().toString())
          )
        )
      );
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + joeBlockId, res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  // https://issues.folio.org/browse/MODUSERS-90
  // https://issues.folio.org/browse/MODUSERS-108
  private Future<Void> putUserWithNumericName(TestContext context) {
    log.info("Changing a user with numeric name\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + user777777Id, res -> {
      RestITSupport.assertStatus(context, res, 404);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> createAddressType(TestContext context) {
    log.info("Creating an address type\n");
    Promise<Void> promise = Promise.promise();
    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "sweethome")
      .put("desc", "The patron's primary residence");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/addresstypes", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(addressTypeObject.encode());
    return promise.future();

  }

  private Future<Void> createBadAddressType(TestContext context) {
    log.info("Creating a bad address type\n");
    Promise<Void> promise = Promise.promise();
    JsonObject addressTypeObject = new JsonObject()
      .put("desc", "The patron's summer residence");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/addresstypes", res -> {
      RestITSupport.assertStatus(context, res, 422);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(addressTypeObject.encode());
    return promise.future();
  }

  private Future<Void> getAddressTypeUpdateUser(TestContext context) {
    log.info("Getting the new addresstype, updating a user with it\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/addresstypes?query=addressType=sweethome", res -> {
      RestITSupport.assertStatus(context, res, 200);
      res.bodyHandler(body -> {
        JsonObject result = new JsonObject(body.toString());
        JsonObject addressType = result.getJsonArray("addressTypes").getJsonObject(0);
        if (!addressType.getString("addressType").equals("sweethome")) {
          promise.fail("addressType is not 'sweethome' in return addresstype");
        } else {
          JsonObject userObject = new JsonObject()
            .put("username", "bobcircle")
            .put("id", bobCircleId)
            .put("active", false)
            .put("personal", new JsonObject()
              .put("lastName", "Circle")
              .put("firstName", "Robert")
              .put("addresses", new JsonArray()
                .add(new JsonObject()
                  .put("countryId", "USA")
                  .put("addressLine1", "123 Somestreet")
                  .put("city", "Somewheresville")
                  .put("addressTypeId", addressType.getString("id"))
                )
              )
            );
          HttpClient putClient = vertx.createHttpClient();
          putClient.put(port, "localhost", "/users/" + bobCircleId, putRes -> {
            RestITSupport.assertStatus(context, putRes, 204);
            HttpClient deleteClient = vertx.createHttpClient();
            deleteClient.delete(port, "localhost", "/addresstypes/"
              + addressType.getString("id"), deleteRes -> {
                RestITSupport.assertStatus(context, deleteRes, 400);
                promise.complete();
            })
              .putHeader("X-Okapi-Tenant", "diku")
              .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
              .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
              .exceptionHandler(promise::fail)
              .end();
          })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
            .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
            .exceptionHandler(promise::fail)
            .end(userObject.encode());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();

  }

  private Future<Void> deleteAddressTypeSQLError(TestContext context) {
    log.info("Deleting address type SQL error\n");
    Promise<Void> promise = Promise.promise();
    HttpClient deleteClient = vertx.createHttpClient();
    deleteClient.delete(port, "localhost", "/addresstypes/x%2F", deleteRes -> {
      RestITSupport.assertStatus(context, deleteRes, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> deleteAddressTypeCQLError(TestContext context) {
    log.info("Deleting address type CQL error\n");
    Promise<Void> promise = Promise.promise();
    HttpClient deleteClient = vertx.createHttpClient();
    deleteClient.delete(port, "localhost", "/addresstypes/x=", deleteRes -> {
      RestITSupport.assertStatus(context, deleteRes, 500);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> createAndDeleteAddressType(TestContext context) {
    log.info("Creating and deleting an address type\n");
    Promise<Void> promise = Promise.promise();
    HttpClient postClient = vertx.createHttpClient();
    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "hardwork")
      .put("desc", "The patron's work address");
    postClient.post(port, "localhost", "/addresstypes", postRes -> {
      RestITSupport.assertStatus(context, postRes, 201);
      postRes.bodyHandler(postBody -> {
        JsonObject newAddressTypeObject = new JsonObject(postBody.toString());
        HttpClient deleteClient = vertx.createHttpClient();
        deleteClient.delete(port, "localhost", "/addresstypes/"
          + newAddressTypeObject.getString("id"), deleteRes -> {
            RestITSupport.assertStatus(context, deleteRes, 204);
            promise.complete();
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
          .exceptionHandler(promise::fail)
          .end();
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .exceptionHandler(promise::fail)
      .end(addressTypeObject.encode());

    return promise.future();
  }

  private Future<Void> postUserWithDuplicateAddressType(TestContext context) {
    log.info("Attempting to create a user with two of the same address types");
    Promise<Void> promise = Promise.promise();
    String addressTypeId = "4716a236-22eb-472a-9f33-d3456c9cc9d5";
    JsonObject userObject = new JsonObject()
      .put("username", "jacktriangle")
      .put("id", jackTriangleId)
      .put("active", true)
      .put("personal", new JsonObject()
        .put("lastName", "Triangle")
        .put("firstName", "Jack")
        .put("addresses", new JsonArray()
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "123 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", addressTypeId)
          )
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "234 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", addressTypeId)
          )
        )
      );
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> postUserWithNotExistingCustomField(TestContext context) {
    log.info("Attempting to create a user with not existing custom field");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
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
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 422);
      res.bodyHandler(err -> {
        Errors errors = Json.decodeValue(err, Errors.class);
        Parameter errorParam = errors.getErrors().get(0).getParameters().get(0);
        context.assertEquals("customFields", errorParam.getKey());
        context.assertEquals(notExistingCustomField, errorParam.getValue());
        promise.complete();
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> postUserBadAddress(TestContext context) {
    log.info("Trying to create a bad address\n");
    Promise<Void> promise = Promise.promise();
    String addressTypeId = "1b1ad9a7-5af5-4545-b5f0-4242ba5f62c8";
    JsonObject userObject = new JsonObject()
      .put("username", "annarhombus")
      .put("id", annaRhombusId)
      .put("active", true)
      .put("personal", new JsonObject()
        .put("lastName", "Rhombus")
        .put("firstName", "Anna")
        .put("addresses", new JsonArray()
          .add(new JsonObject()
            .put("countryId", "USA")
            .put("addressLine1", "456 Somestreet")
            .put("city", "Somewheresville")
            .put("addressTypeId", addressTypeId)
          )
        )
      );
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 400);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

  private Future<Void> postUserWithDuplicateId(TestContext context) {
    log.info("Attempting to create a user with duplicate id");
    Promise<Void> promise = Promise.promise();
    String uuid = UUID.randomUUID().toString();
    JsonObject user1 = new JsonObject().put("id", uuid);
    JsonObject user2 = new JsonObject().put("id", uuid);
    HttpClient client = vertx.createHttpClient();
    // create test user one
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      // fail attempting to create user with duplicate id
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 422);
        res2.bodyHandler(err -> {
          JsonObject validationErrorRes = err.toJsonObject();
          JsonArray validationErrors = validationErrorRes.getJsonArray("errors");
          if (validationErrors.isEmpty()) {
            promise.fail("Did not return expected validation errors");
          } else {
            String errorMessage = validationErrors.getJsonObject(0).getString("message");
            context.assertEquals(1, validationErrors.size());
            context.assertEquals(errorMessage, "User with this id already exists");
            promise.complete();
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(user2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(user1.encode());
    return promise.future();
  }

  private Future<Void> postUserWithDuplicateUsername(TestContext context) {
    log.info("Attempting to create a user with duplicate username");
    Promise<Void> promise = Promise.promise();
    JsonObject user1 = new JsonObject()
      .put("username", "the_twin")
      .put("id",  UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("username", "the_twin")
      .put("id",  UUID.randomUUID().toString());
    HttpClient client = vertx.createHttpClient();
    // create test user one
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      // creating a second user with the same username should fail
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 422);
        res2.bodyHandler(err -> {
          JsonObject validationErrorRes = err.toJsonObject();
          JsonArray validationErrors = validationErrorRes.getJsonArray("errors");
          if (validationErrors.isEmpty()) {
            promise.fail("Did not return expected validation errors");
          } else {
            String errorMessage = validationErrors.getJsonObject(0).getString("message");
            context.assertEquals(1, validationErrors.size());
            context.assertEquals(errorMessage, "User with this username already exists");
            promise.complete();
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(user2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(user1.encode());
    return promise.future();
  }

  private Future<Void> putUserWithDuplicateUsername(TestContext context) {
    log.info("Changing a user to username that already exists\n");
    Promise<Void> promise = Promise.promise();
    JsonObject user1 = new JsonObject()
      .put("username", "left_shoe")
      .put("id", UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("username", "right_shoe")
      .put("id", UUID.randomUUID().toString());
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 201);
        // attempt to update user2 changing username to a duplicate
        user2.put("username", "left_shoe");
        client.put(port, "localhost", "/users/" + user2.getString("id"), res3 -> {
          RestITSupport.assertStatus(context, res3, 400);
          res3.bodyHandler(err -> {
            context.assertEquals("User with this username already exists", err.toString());
            promise.complete();
          });
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
          .exceptionHandler(promise::fail)
          .end(user2.encode());
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(user2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(user1.encode());
    return promise.future();
  }

  // https://issues.folio.org/browse/MODUSERS-147
  private Future<Void> postTwoUsersWithoutUsername(TestContext context) {
    log.info("Attempting to create two users without username");
    Promise<Void> promise = Promise.promise();
    JsonObject user1 = new JsonObject()
      .put("id",  UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("id",  UUID.randomUUID().toString());
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      client.post(port, "localhost", "/users", res2 -> {
        // should succeed, there can be any number of users without username
        RestITSupport.assertStatus(context, res2, 201);
        promise.complete();
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(user2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(user1.encode());
    return promise.future();
  }

  // https://issues.folio.org/browse/MODUSERS-147
  private Future<Void> putSecondUserWithoutUsername(TestContext context) {
    log.info("Updating second user to have no username");
    Promise<Void> promise = Promise.promise();
    JsonObject user1 = new JsonObject()
      .put("id", UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
        .put("username", "name_for_sale")
        .put("id", UUID.randomUUID().toString());
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 201);
        user2.remove("username");  // try to PUT with username removed
        client.put(port, "localhost", "/users/" + user2.getString("id"), res3 -> {
          RestITSupport.assertStatus(context, res3, 204);
          promise.complete();
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
          .exceptionHandler(promise::fail)
          .end(user2.encode());
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(user2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(user1.encode());
    return promise.future();
  }

  // https://issues.folio.org/browse/MODUSERS-118
  private Future<Void> postUserWithDuplicateBarcode(TestContext context) {
    log.info("Attempting to create a user with duplicate barcode");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject1 = new JsonObject()
      .put("username", "test_one")
      .put("id",  UUID.randomUUID().toString())
      .put("active", true)
      .put("barcode", "943259854978643")
      .put("personal", new JsonObject()
        .put("lastName", "One")
        .put("firstName", "Test")
      );
    JsonObject userObject2 = new JsonObject()
      .put("username", "test_two")
      .put("id",  UUID.randomUUID().toString())
      .put("active", true)
      .put("barcode", "943259854978643")
      .put("personal", new JsonObject()
        .put("lastName", "Two")
        .put("firstName", "Test")
      );
    HttpClient client = vertx.createHttpClient();
    // create test user one
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      // fail attempting to create user with duplicate barcode
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 422);
        res2.bodyHandler(err -> {
          JsonObject validationErrorRes = err.toJsonObject();
          JsonArray validationErrors = validationErrorRes.getJsonArray("errors");
          if (validationErrors.isEmpty()) {
            promise.fail("Did not return expected validation errors");
          } else {
            String errorMessage = validationErrors.getJsonObject(0).getString("message");
            assertThat(1, is(validationErrors.size()));
            assertThat(errorMessage, is("This barcode has already been taken"));
            promise.complete();
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(userObject2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject1.encode());
    return promise.future();
  }

  // https://issues.folio.org/browse/MODUSERS-118
  private Future<Void> putUserWithDuplicateBarcode(TestContext context) {
    log.info("Changing a user to barcode that already exists\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject1 = new JsonObject()
      .put("username", "test_three")
      .put("id", UUID.randomUUID().toString())
      .put("active", true)
      .put("barcode", "304276530498752")
      .put("personal", new JsonObject()
        .put("lastName", "Three")
        .put("firstName", "Test")
      );
    String testUserFourId = UUID.randomUUID().toString();
    JsonObject userObject2 = new JsonObject()
      .put("username", "test_four")
      .put("id", testUserFourId)
      .put("active", true)
      .put("barcode", "098743509873450")
      .put("personal", new JsonObject()
        .put("lastName", "Four")
        .put("firstName", "Test")
      );
    HttpClient client = vertx.createHttpClient();
    // create test user one
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      // create test user two
      client.post(port, "localhost", "/users", res2 -> {
        RestITSupport.assertStatus(context, res2, 201);
        // fail attempting to update user changing barcode to a duplicate
        userObject2.put("barcode", "304276530498752");
        client.put(port, "localhost", "/users/" + testUserFourId, res3 -> {
          RestITSupport.assertStatus(context, res3, 400);
          res3.bodyHandler(err -> {
            String errorMessage = err.toString();
            assertThat(errorMessage, is("This barcode has already been taken"));
            promise.complete();
          });
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
          .exceptionHandler(promise::fail)
          .end(userObject2.encode());
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end(userObject2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject1.encode());
    return promise.future();
  }

  private Future<Void> createProxyfor(TestContext context) {
    log.info("Creating a new proxyfor entry\n");
    Promise<Void> promise = Promise.promise();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(proxyObject.encode());
    return promise.future();
  }

  private Future<Void> createProxyforWithSameUserId(TestContext context) {
    log.info("Trying to create a proxyfor with an existing userid\n");
    Promise<Void> promise = Promise.promise();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "5b0a9a0b-6eb6-447c-bc31-9c99940a29c5");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(proxyObject.encode());

    return promise.future();
  }

  private Future<Void> createProxyforWithSameProxyUserId(TestContext context) {
    log.info("Trying to create a proxyfor with an existing proxy userid\n");
    Promise<Void> promise = Promise.promise();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "bd2cbc13-9d43-4a74-8090-75bc4e26a8df")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(proxyObject.encode());
    return promise.future();
  }

  private Future<Void> failToCreateDuplicateProxyfor(TestContext context) {
    log.info("Trying to create a proxyfor entry with the same id and proxy user id\n");
    Promise<Void> promise = Promise.promise();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      RestITSupport.assertStatus(context, res, 422);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(proxyObject.encode());
    return promise.future();
  }

  private Future<Void> getProxyforCollection(TestContext context) {
    log.info("Getting proxyfor entries\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/proxiesfor", res -> {
      RestITSupport.assertStatus(context, res, 200);
      res.bodyHandler(body -> {
        JsonObject resultJson = body.toJsonObject();
        JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
        if (proxyForArray.size() == 3) {
          promise.complete();
        } else {
          promise.fail("Expected 3 entries, found " + proxyForArray.size());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  private Future<Void> findAndGetProxyfor(TestContext context) {
    log.info("Find and retrieve a particular proxyfor entry\n");
    Promise<Void> promise = Promise.promise();
    try {
      HttpClient client = vertx.createHttpClient();
      log.info("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          RestITSupport.assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                promise.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              log.info("Making get-by-id request\n");
              client.get(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                RestITSupport.assertStatus(context, res2, 200);
                promise.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .exceptionHandler(promise::fail)
                .end();
            } catch (Exception e) {
              promise.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> findAndUpdateProxyfor(TestContext context) {
    log.info("Find and update a particular proxyfor entry\n");
    Promise<Void> promise = Promise.promise();
    JsonObject modifiedProxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    try {
      HttpClient client = vertx.createHttpClient();
      log.info("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          RestITSupport.assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                promise.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              log.info("Making put-by-id request\n");
              client.put(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                RestITSupport.assertStatus(context, res2, 204);
                promise.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
                .exceptionHandler(promise::fail)
                .end(modifiedProxyObject.encode());
            } catch (Exception e) {
              promise.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> findAndDeleteProxyfor(TestContext context) {
    log.info("Find and delete a particular proxyfor entry");
    Promise<Void> promise = Promise.promise();
    try {
      HttpClient client = vertx.createHttpClient();
      log.info("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          RestITSupport.assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                promise.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              log.info("Making delete-by-id request\n");
              client.delete(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                RestITSupport.assertStatus(context, res2, 204);
                promise.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
                .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
                .exceptionHandler(promise::fail)
                .end();
            } catch (Exception e) {
              promise.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
        .exceptionHandler(promise::fail)
        .end();
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> createTestDeleteObjectById(TestContext context, JsonObject ob,
    String endpoint, boolean checkMeta) {
    Promise<Void> promise = Promise.promise();
    log.info(String.format(
      "Creating object %s at endpoint %s", ob.encode(), endpoint));
    HttpClient client = vertx.createHttpClient();
    String fakeJWT = makeFakeJWT("bubba", UUID.randomUUID().toString(), "diku");
    client.post(port, "localhost", endpoint, res -> {
      RestITSupport.assertStatus(context, res, 201);
      res.bodyHandler(body -> {
        //Get the object by id
        String id = body.toJsonObject().getString("id");
        client.get(port, "localhost", endpoint + "/" + id, res2 -> {
          RestITSupport.assertStatus(context, res2, 200);
          res2.bodyHandler(body2 -> {
            if (checkMeta) {
              DateFormat gmtFormat = new SimpleDateFormat(
                "yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'");
              Date createdDate = null;
              try {
                JsonObject resultOb = body2.toJsonObject();
                JsonObject metadata = resultOb.getJsonObject("metadata");
                if (metadata == null) {
                  promise.fail(String.format("No 'metadata' field in result: %s",
                    body2.toString()));
                  return;
                }
                createdDate = new DateTime(metadata.getString("createdDate")).toDate();
              } catch (Exception e) {
                promise.fail(e);
                return;
              }
              Date now = new Date();
              if (!createdDate.before(now)) {
                promise.fail("metadata createdDate is not correct");
                return;
              }
            }
            //delete the object by id
            client.delete(port, "localhost", endpoint + "/" + id, res3 -> {
              RestITSupport.assertStatus(context, res3, 204);
              promise.complete();
            })
              .putHeader("X-Okapi-Tenant", "diku")
              .putHeader("Content-Type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
              .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
              .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
              .exceptionHandler(promise::fail)
              .end();
          });
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("Content-Type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
          .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
          .exceptionHandler(promise::fail)
          .end();

      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-Type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("Accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .putHeader("X-Okapi-Token", fakeJWT)
      .exceptionHandler(promise::fail)
      .end(ob.encode());
    return promise.future();
  }

  private Future<Void> getGroupByInvalidUuid(TestContext context) {
    log.info("Retrieving a group by invalid uuid\n");
    Promise<Void> promise = Promise.promise();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/groups/q", res -> {
      RestITSupport.assertStatus(context, res, 404);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end();
    return promise.future();
  }

  @Test
  public void test1Sequential(TestContext context) {
    /**
     * The CQL used for searching when a single j has been entered into the
     * search slot
     */
    final String jSearch = "(((username=\"j*\" or personal.firstName=\"j*\" or "
      + "personal.lastName=\"j*\" or personal.email=\"j*\" or barcode=\"j*\" or "
      + "id=\"j*\" or externalSystemId=\"j*\")) and active=\"true\") "
      + "sortby personal.lastName personal.firstName";

    Async async = context.async();
    Future<Void> startFuture;
    startFuture = getEmptyUsers(context)
      .compose(v -> postUser(context, false))
      .compose(v -> putUserGood(context, joeBlockId, false))
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> postUser(context, true))
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> postUser(context, true))
      .compose(v -> postUserWithCustomFields(context))
      .compose(v -> getUserWithCustomFields(context))
      .compose(v -> deleteUser(context, johnRectangleId))
      .compose(v -> getUser(context))
      .compose(v -> getUserByCQL(context))
      .compose(v -> getUserByCqlById(context))
      .compose(v -> getUserByInvalidCQL(context))
      .compose(v -> deleteNonExistingUser(context))
      .compose(v -> postAnotherUser(context))
      .compose(v -> getUsersByCQL(context, "id==x") /* empty result */)
      .compose(v -> getUsersByCQL(context, "id==\"\"", "bobcircle", "joeblock"))
      .compose(v -> getUsersByCQL(context, jSearch, "joeblock"))
      .compose(v -> putUserGood(context, bobCircleId, true))
      .compose(v -> putUserBadUsername(context))
      .compose(v -> putUserWithoutIdInMetadata(context))
      .compose(v -> putUserWithNotExistingCustomField(context))
      .compose(v -> getGoodUser(context))
      .compose(v -> putUserBadId(context))
      .compose(v -> putUserNotMatchingId(context))
      .compose(v -> putUserDuplicatedAddressType(context))
      .compose(v -> putUserInvalidAddressType(context))
      .compose(v -> putUserWithNumericName(context))
      .compose(v -> putUserWithDuplicateUsername(context))
      .compose(v -> putUserWithDuplicateBarcode(context))
      .compose(v -> createAddressType(context))
      .compose(v -> createBadAddressType(context))
      .compose(v -> getAddressTypeUpdateUser(context))
      .compose(v -> createAndDeleteAddressType(context))
      .compose(v -> deleteAddressTypeSQLError(context))
      .compose(v -> deleteAddressTypeCQLError(context))
      .compose(v -> postUserWithDuplicateAddressType(context))
      .compose(v -> postUserBadAddress(context))
      .compose(v -> postUserWithNumericName(context))
      .compose(v -> postUserWithDuplicateId(context))
      .compose(v -> postUserWithDuplicateUsername(context))
      .compose(v -> postUserWithNotExistingCustomField(context))
      .compose(v -> postTwoUsersWithoutUsername(context))
      .compose(v -> putSecondUserWithoutUsername(context))
      .compose(v -> postUserWithDuplicateBarcode(context))
      .compose(v -> createProxyfor(context))
      .compose(v -> createProxyforWithSameUserId(context))
      .compose(v -> createProxyforWithSameProxyUserId(context))
      .compose(v -> failToCreateDuplicateProxyfor(context))
      .compose(v -> getProxyforCollection(context))
      .compose(v -> findAndGetProxyfor(context))
      .compose(v -> findAndUpdateProxyfor(context))
      .compose(v -> findAndDeleteProxyfor(context))
      .compose(v -> createTestDeleteObjectById(context, testAddress, "/addresstypes", true))
      .compose(v -> createTestDeleteObjectById(context, testGroup, "/groups", true))
      .compose(v -> createTestDeleteObjectById(context, testProxyFor, "/proxiesfor", true));

    // It hung after 5-12 invocations. MODUSERS-100
    for (int i = 0; i < 25; i++) {
      startFuture = startFuture.compose(v -> getGroupByInvalidUuid(context));
    }

    startFuture.setHandler(res -> {
      if (res.succeeded()) {
        async.complete();
      } else {
        res.cause().printStackTrace();
        context.fail(res.cause());
      }
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

  @Test
  public void test5UserName(TestContext context) {
    Async async = context.async();
    postUserWithWhitespace(context)
      .compose(v -> getUsersByCQL(context, String.format("id==%s", userIdWithWhitespace), "user name"))
      .compose(v -> deleteUser(context, userIdWithWhitespace))
      .setHandler(res -> {
        if (res.succeeded()) {
          async.complete();
        } else {
          res.cause().printStackTrace();
          context.fail(res.cause());
        }
      });
  }

  private Future<Void> postUserWithWhitespace(TestContext context) {
    log.info("Creating a user with a numeric name\n");
    Promise<Void> promise = Promise.promise();
    JsonObject userObject = new JsonObject()
      .put("username", " user name ")
      .put("id", userIdWithWhitespace)
      .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      RestITSupport.assertStatus(context, res, 201);
      promise.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .exceptionHandler(promise::fail)
      .end(userObject.encode());
    return promise.future();
  }

}
