package org.folio.moduserstest;

import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientRequest;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import junit.framework.AssertionFailedError;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertThat;

import java.net.HttpURLConnection;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.tools.parser.JsonPathParser;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.tools.utils.VertxUtils;
import org.folio.rest.utils.ExpirationTool;
import org.folio.util.StringUtil;
import org.joda.time.DateTime;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestVerticleIT {

  private static final String SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";

  private static final String fooGroupData = "{\"group\": \"librarianFOO\",\"desc\": \"yet another basic lib group\"}";
  private static final String barGroupData = "{\"group\": \"librarianBAR\",\"desc\": \"and yet another basic lib group\"}";

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String bobCircleId = "54afd8b8-fb3b-4de8-9b7c-299904887f7d";
  private static final String jackTriangleId = "e133841d-b645-4488-9e52-9762d560b617";
  private static final String annaRhombusId = "e8090974-8876-4411-befa-8ddcffad0b35";
  private static final String user777777Id = "72bd29f7-bf29-48bb-8259-d5ce78378a56";

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
  private static Context vertxContext;
  static int port;

  private String groupID1;

  @Rule
  public Timeout rule = Timeout.seconds(20);

  private static void fail(TestContext context, HttpClientResponse response) {
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this fail method from the stacktrace
    fail(context, null, response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  private static void fail(TestContext context, String message, HttpClientResponse response) {
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this fail method from the stacktrace
    fail(context, message, response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  private static void fail(TestContext context, String message, HttpClientResponse response,
      StackTraceElement [] stacktrace) {
    Async async = context.async();
    response.bodyHandler(body -> {
      Throwable t = new AssertionFailedError((message == null ? "" : message + ": ")
          + response.statusCode() + " " + response.statusMessage() + " " + body.toString());
      // t contains the stacktrace of bodyHandler but does not contain the method that
      // called this fail method. Therefore exchange the stacktrace:
      t.setStackTrace(stacktrace);
      context.fail(t);
      async.complete();
    });
  }

  /**
   * Fail the context if response does not have the provided status.
   */
  private static void assertStatus(TestContext context, HttpClientResponse response, int status) {
    if (response.statusCode() == status) {
      return;
    }
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this assertStatus method from the stacktrace
    fail(context, "Expected status " + status + " but got",
        response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  @BeforeClass
  public static void setup(TestContext context) throws SQLException {
    vertx = VertxUtils.getVertxWithExceptionHandler();
    vertxContext = vertx.getOrCreateContext();

    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("http://localhost:" + Integer.toString(port), "diku", "diku");
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
            fail(context, "deleteTenant", delete);
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
                fail(context, "postTenant", post);
              }
              async.complete();
            });
          } catch (Exception e) {
            context.fail(e);
          }
        });
    }));
  }

  private Future<Void> getEmptyUsers(TestContext context) {
    System.out.println("Getting an empty user set\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users", res -> {
      assertStatus(context, res, 200);
      res.bodyHandler(buf -> {
        JsonObject userCollectionObject = buf.toJsonObject();
        if (userCollectionObject.getJsonArray("users").size() == 0
          && userCollectionObject.getInteger("totalRecords") == 00) {
          future.complete();
        } else {
          future.fail("Invalid return JSON: " + buf.toString());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private static void addTags(JsonObject u) {
    JsonArray tagList = new JsonArray();
    tagList.add("foo-tag");
    tagList.add("bar-tag");
    JsonObject tagobj = new JsonObject();
    tagobj.put("tagList", tagList);
    u.put("tags", tagobj);
  }

  private Future<Void> postUser(TestContext context) {
    System.out.println("Creating a new user\n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", joeBlockId)
      .put("active", true);
    addTags(userObject);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if (res.statusCode() >= 200 && res.statusCode() < 300) {
        future.complete();
      } else {
        future.fail("Got status code: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> deleteNonExistingUser(TestContext context) {
    System.out.println("Deleting non-existing user\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.delete(port, "localhost", "/users/85936906-4737-4da7-b0fb-e8da080b97d8", res -> {
      assertStatus(context, res, 404);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("accept", "*/*")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> deleteUser(TestContext context) {
    System.out.println("Deleting existing user\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.delete(port, "localhost", "/users/" + joeBlockId, res -> {
      assertStatus(context, res, 204);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("accept", "*/*")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> postUserWithNumericName(TestContext context) {
    System.out.println("Creating a user with a numeric name\n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      assertStatus(context, res, 201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> getUser(TestContext context) {
    System.out.println("Retrieving a user\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users/" + joeBlockId, res -> {
      if (res.statusCode() == 200) {
        res.bodyHandler(buf -> {
          JsonObject userObject = buf.toJsonObject();
          if (userObject.getString("username").equals("joeblock")) {
            JsonObject tags = userObject.getJsonObject("tags");
            if (tags == null || !tags.encode().equals("{\"tagList\":[\"foo-tag\",\"bar-tag\"]}")) {
              future.fail("Bad value for tag list. " + buf.toString());
            } else {
              DateFormat gmtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'");
              Date createdDate = null;
              try {
                //createdDate = DatatypeConverter.parseDateTime(userObject.getString("createdDate")).getTime();
                createdDate = new DateTime(userObject.getString("createdDate")).toDate();
              } catch (Exception e) {
                future.fail(e);
                return;
              }
              Date now = new Date();
              if (createdDate.before(now)) {
                future.complete();
              } else {
                future.fail("Bad value for createdDate");
              }
            }
          } else {
            future.fail("Unable to read proper data from JSON return value: " + buf.toString());
          }
        });
      } else {
        future.fail("Bad response: " + res.statusCode());
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> getUserByCQL(TestContext context) {
    System.out.println("Getting user via CQL, by username\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + StringUtil.urlEncode("username==joeblock"), res -> {
        assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            if (totalRecords != 1) {
              future.fail("Expected 1 record, got " + totalRecords);
              return;
            }
            JsonArray userList = resultObject.getJsonArray("users");
            JsonObject userObject = userList.getJsonObject(0);
            if (userObject.getString("username").equals("joeblock")) {
              future.complete();
            } else {
              future.fail("Unable to read proper data from JSON return value: " + buf.toString());
            }
          } catch (Exception e) {
            future.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> getUserByCqlById(TestContext context) {
    System.out.println("Getting user via CQL, by user id\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + StringUtil.urlEncode("(id==" + joeBlockId + ")"), res -> {
        assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            assertThat(totalRecords, is(1));
            JsonArray userList = resultObject.getJsonArray("users");
            JsonObject userObject = userList.getJsonObject(0);
            assertThat("username of " + buf, userObject.getString("username"), is("joeblock"));
            future.complete();
          } catch (Exception e) {
            future.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> getUserByInvalidCQL(TestContext context) {
    System.out.println("Getting user via invalid CQL\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    try {
      // empty CQL query triggers parse exception
      client.get(port, "localhost", "/users?query=", res -> {
        assertStatus(context, res, 400);
        future.complete();
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> postAnotherUser(TestContext context) {
    System.out.println("Creating another user\n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "bobcircle")
      .put("id", bobCircleId)
      .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      assertStatus(context, res,  201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> getUsersByCQL(TestContext context, String cql, String... expectedUsernames) {
    System.out.println("Query users via CQL\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    try {
      client.get(port, "localhost", "/users?query=" + StringUtil.urlEncode(cql), res -> {
        assertStatus(context, res, 200);
        res.bodyHandler(buf -> {
          try {
            JsonObject resultObject = buf.toJsonObject();
            int totalRecords = resultObject.getInteger("totalRecords");
            JsonArray userList = resultObject.getJsonArray("users");
            if (userList.size() != totalRecords) {
              future.fail("totalRecords=" + totalRecords + " mismatch users list: " + userList.encodePrettily());
              return;
            }
            List<String> usernames = new ArrayList<>();
            for (int i = 0; i < userList.size(); i++) {
              usernames.add(userList.getJsonObject(i).getString("username"));
            }
            assertThat(usernames, containsInAnyOrder(expectedUsernames));
            future.complete();
          } catch (Exception e) {
            future.fail(e);
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> putUserGood(TestContext context) {
    System.out.println("Making a valid user modification\n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "bobcircle")
      .put("id", bobCircleId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + bobCircleId, res -> {
      assertStatus(context, res, 204);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> getGoodUser(TestContext context) {
    System.out.println("Getting the modified user\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users/" + bobCircleId, res -> {
      assertStatus(context, res, 200);
      res.bodyHandler(buf -> {
        JsonObject userObject = buf.toJsonObject();
        if (userObject.getString("username").equals("bobcircle")) {
          Date createdDate = null;
          Date updatedDate = null;
          try {
            createdDate = new DateTime(userObject.getString("createdDate")).toDate();
            updatedDate = new DateTime(userObject.getString("updatedDate")).toDate();
          } catch (Exception e) {
            future.fail(e);
            return;
          }
          Date now = new Date();
          if (createdDate.before(now) && updatedDate.before(now) && createdDate.before(updatedDate)) {
            future.complete();
          } else {
            future.fail("Bad value for createdDate and/or updatedDate");
          }
        } else {
          future.fail("Unable to read proper data from JSON return value: " + buf.toString());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> putUserBadUsername(TestContext context) {
    System.out.println("Trying to assign an invalid username \n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "joeblock")
      .put("id", bobCircleId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + bobCircleId, res -> {
      assertStatus(context, res, 400);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> putUserBadId(TestContext context) {
    System.out.println("Trying to assign an invalid id \n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "bobcircle")
      .put("id", joeBlockId)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + joeBlockId, res -> {
      assertStatus(context, res, 400);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  // https://issues.folio.org/browse/MODUSERS-90
  // https://issues.folio.org/browse/MODUSERS-108
  private Future<Void> putUserWithNumericName(TestContext context) {
    System.out.println("Changing a user with numeric name\n");
    Future<Void> future = Future.future();
    JsonObject userObject = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/" + user777777Id, res -> {
      assertStatus(context, res, 404);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> createAddressType(TestContext context) {
    System.out.println("Creating an address type\n");
    Future<Void> future = Future.future();
    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "sweethome")
      .put("desc", "The patron's primary residence");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/addresstypes", res -> {
      assertStatus(context, res, 201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(addressTypeObject.encode());
    return future;

  }

  private Future<Void> createBadAddressType(TestContext context) {
    System.out.println("Creating a bad address type\n");
    Future<Void> future = Future.future();
    JsonObject addressTypeObject = new JsonObject()
      .put("desc", "The patron's summer residence");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/addresstypes", res -> {
      assertStatus(context, res, 422);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(addressTypeObject.encode());
    return future;
  }

  private Future<Void> getAddressTypeUpdateUser(TestContext context) {
    System.out.println("Getting the new addresstype, updating a user with it\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/addresstypes?query=addressType=sweethome", res -> {
      assertStatus(context, res, 200);
      res.bodyHandler(body -> {
        JsonObject result = new JsonObject(body.toString());
        JsonObject addressType = result.getJsonArray("addressTypes").getJsonObject(0);
        if (!addressType.getString("addressType").equals("sweethome")) {
          future.fail("addressType is not 'sweethome' in return addresstype");
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
            assertStatus(context, putRes, 204);
            HttpClient deleteClient = vertx.createHttpClient();
            deleteClient.delete(port, "localhost", "/addresstypes/"
              + addressType.getString("id"), deleteRes -> {
                assertStatus(context, deleteRes, 400);
                future.complete();
            })
              .putHeader("X-Okapi-Tenant", "diku")
              .putHeader("content-type", "application/json")
              .putHeader("accept", "text/plain")
              .exceptionHandler(e -> {
                future.fail(e);
              })
              .end();
          })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .exceptionHandler(e -> {
              future.fail(e);
            })
            .end(userObject.encode());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;

  }

  private Future<Void> deleteAddressTypeSQLError(TestContext context) {
    System.out.println("Deleting address type SQL error\n");
    Future<Void> future = Future.future();
    HttpClient deleteClient = vertx.createHttpClient();
    deleteClient.delete(port, "localhost", "/addresstypes/x%2F", deleteRes -> {
      assertStatus(context, deleteRes, 400);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> deleteAddressTypeCQLError(TestContext context) {
    System.out.println("Deleting address type CQL error\n");
    Future<Void> future = Future.future();
    HttpClient deleteClient = vertx.createHttpClient();
    deleteClient.delete(port, "localhost", "/addresstypes/x=", deleteRes -> {
      assertStatus(context, deleteRes, 500);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> createAndDeleteAddressType(TestContext context) {
    System.out.println("Creating and deleting an address type\n");
    Future<Void> future = Future.future();
    HttpClient postClient = vertx.createHttpClient();
    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "hardwork")
      .put("desc", "The patron's work address");
    postClient.post(port, "localhost", "/addresstypes", postRes -> {
      assertStatus(context, postRes, 201);
      postRes.bodyHandler(postBody -> {
        JsonObject newAddressTypeObject = new JsonObject(postBody.toString());
        HttpClient deleteClient = vertx.createHttpClient();
        deleteClient.delete(port, "localhost", "/addresstypes/"
          + newAddressTypeObject.getString("id"), deleteRes -> {
            assertStatus(context, deleteRes, 204);
            future.complete();
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", "application/json")
          .putHeader("accept", "text/plain")
          .exceptionHandler(e -> {
            future.fail(e);
          })
          .end();
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(addressTypeObject.encode());

    return future;
  }

  private Future<Void> postUserWithDuplicateAddressType(TestContext context) {
    System.out.println("Attempting to create a user with two of the same address types");
    Future<Void> future = Future.future();
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
      assertStatus(context, res, 400);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> postUserBadAddress(TestContext context) {
    System.out.println("Trying to create a bad address\n");
    Future<Void> future = Future.future();
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
      assertStatus(context, res, 400);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject.encode());
    return future;
  }

  private Future<Void> postUserWithDuplicateBarcode(TestContext context) {
    Future<Void> future = Future.future();
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
      assertStatus(context, res, 201);
      // fail attempting to create user with duplicate barcode
      client.post(port, "localhost", "/users", res2 -> {
        assertStatus(context, res2, 422);
        res2.bodyHandler(err -> {
          JsonObject validationErrorRes = err.toJsonObject();
          JsonArray validationErrors = validationErrorRes.getJsonArray("errors");
          if (validationErrors.isEmpty()) {
            future.fail("Did not return expected validation errors");
          } else {
            String errorMessage = validationErrors.getJsonObject(0).getString("message");
            assertThat(1, is(validationErrors.size()));
            assertThat(errorMessage, is("This barcode has already been taken"));
            future.complete();
          }
        });
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end(userObject2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject1.encode());
    return future;
  }

  private Future<Void> putUserWithDuplicateBarcode(TestContext context) {
    Future<Void> future = Future.future();
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
      assertStatus(context, res, 201);
      // create test user two
      client.post(port, "localhost", "/users", res2 -> {
        assertStatus(context, res2, 201);
        // fail attempting to update user changing barcode to a duplicate
        userObject2.put("barcode", "304276530498752");
        client.put(port, "localhost", "/users/" + testUserFourId, res3 -> {
          assertStatus(context, res3, 400);
          res3.bodyHandler(err -> {
            String errorMessage = err.toString();
            assertThat(errorMessage, is("This barcode has already been taken"));
            future.complete();
          });
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("content-type", "application/json")
          .putHeader("accept", "text/plain")
          .exceptionHandler(e -> {
            future.fail(e);
          })
          .end(userObject2.encode());
      })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end(userObject2.encode());
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(userObject1.encode());
    return future;
  }

  private Future<Void> createProxyfor(TestContext context) {
    System.out.println("Creating a new proxyfor entry\n");
    Future<Void> future = Future.future();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      assertStatus(context, res, 201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(proxyObject.encode());
    return future;
  }

  private Future<Void> createProxyforWithSameUserId(TestContext context) {
    System.out.println("Trying to create a proxyfor with an existing userid\n");
    Future<Void> future = Future.future();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "5b0a9a0b-6eb6-447c-bc31-9c99940a29c5");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      assertStatus(context, res, 201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(proxyObject.encode());

    return future;
  }

  private Future<Void> createProxyforWithSameProxyUserId(TestContext context) {
    System.out.println("Trying to create a proxyfor with an existing proxy userid\n");
    Future<Void> future = Future.future();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "bd2cbc13-9d43-4a74-8090-75bc4e26a8df")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      assertStatus(context, res, 201);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(proxyObject.encode());
    return future;
  }

  private Future<Void> failToCreateDuplicateProxyfor(TestContext context) {
    System.out.println("Trying to create a proxyfor entry with the same id and proxy user id\n");
    Future<Void> future = Future.future();
    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/proxiesfor", res -> {
      assertStatus(context, res, 422);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(proxyObject.encode());
    return future;
  }

  private Future<Void> getProxyforCollection(TestContext context) {
    System.out.println("Getting proxyfor entries\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/proxiesfor", res -> {
      assertStatus(context, res, 200);
      res.bodyHandler(body -> {
        JsonObject resultJson = body.toJsonObject();
        JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
        if (proxyForArray.size() == 3) {
          future.complete();
        } else {
          future.fail("Expected 3 entries, found " + proxyForArray.size());
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
  }

  private Future<Void> findAndGetProxyfor(TestContext context) {
    System.out.println("Find and retrieve a particular proxyfor entry\n");
    Future<Void> future = Future.future();
    try {
      HttpClient client = vertx.createHttpClient();
      System.out.println("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                future.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              System.out.println("Making get-by-id request\n");
              client.get(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                assertStatus(context, res2, 200);
                future.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", "application/json")
                .putHeader("accept", "application/json")
                .exceptionHandler(e -> {
                  future.fail(e);
                })
                .end();
            } catch (Exception e) {
              future.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> findAndUpdateProxyfor(TestContext context) {
    System.out.println("Find and update a particular proxyfor entry\n");
    Future<Void> future = Future.future();
    JsonObject modifiedProxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");
    try {
      HttpClient client = vertx.createHttpClient();
      System.out.println("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                future.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              System.out.println("Making put-by-id request\n");
              client.put(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                assertStatus(context, res2, 204);
                future.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", "application/json")
                .putHeader("accept", "application/json")
                .putHeader("accept", "text/plain")
                .exceptionHandler(e -> {
                  future.fail(e);
                })
                .end(modifiedProxyObject.encode());
            } catch (Exception e) {
              future.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> findAndDeleteProxyfor(TestContext context) {
    Future<Void> future = Future.future();
    try {
      HttpClient client = vertx.createHttpClient();
      System.out.println("Making CQL request\n");
      client.get(port, "localhost",
        "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa",
        res -> {
          assertStatus(context, res, 200);
          res.bodyHandler(body -> {
            try {
              JsonObject resultJson = body.toJsonObject();
              JsonArray proxyForArray = resultJson.getJsonArray("proxiesFor");
              if (proxyForArray.size() != 1) {
                future.fail("Expected 1 entry, found " + proxyForArray.size());
                return;
              }
              JsonObject proxyForObject = proxyForArray.getJsonObject(0);
              String proxyForId = proxyForObject.getString("id");
              System.out.println("Making delete-by-id request\n");
              client.delete(port, "localhost", "/proxiesfor/" + proxyForId, res2 -> {
                assertStatus(context, res2, 204);
                future.complete();
              })
                .putHeader("X-Okapi-Tenant", "diku")
                .putHeader("content-type", "application/json")
                .putHeader("accept", "application/json")
                .putHeader("accept", "text/plain")
                .exceptionHandler(e -> {
                  future.fail(e);
                })
                .end();
            } catch (Exception e) {
              future.fail(e);
            }
          });
        })
        .putHeader("X-Okapi-Tenant", "diku")
        .putHeader("content-type", "application/json")
        .putHeader("accept", "application/json")
        .exceptionHandler(e -> {
          future.fail(e);
        })
        .end();
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

  private Future<Void> createTestDeleteObjectById(TestContext context, JsonObject ob,
    String endpoint, boolean checkMeta) {
    Future<Void> future = Future.future();
    System.out.println(String.format(
      "Creating object %s at endpoint %s", ob.encode(), endpoint));
    HttpClient client = vertx.createHttpClient();
    String fakeJWT = makeFakeJWT("bubba", UUID.randomUUID().toString(), "diku");
    client.post(port, "localhost", endpoint, res -> {
      assertStatus(context, res, 201);
      res.bodyHandler(body -> {
        //Get the object by id
        String id = body.toJsonObject().getString("id");
        client.get(port, "localhost", endpoint + "/" + id, res2 -> {
          assertStatus(context, res2, 200);
          res2.bodyHandler(body2 -> {
            if (checkMeta) {
              DateFormat gmtFormat = new SimpleDateFormat(
                "yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'");
              Date createdDate = null;
              try {
                JsonObject resultOb = body2.toJsonObject();
                JsonObject metadata = resultOb.getJsonObject("metadata");
                if (metadata == null) {
                  future.fail(String.format("No 'metadata' field in result: %s",
                    body2.toString()));
                  return;
                }
                createdDate = new DateTime(metadata.getString("createdDate")).toDate();
              } catch (Exception e) {
                future.fail(e);
                return;
              }
              Date now = new Date();
              if (!createdDate.before(now)) {
                future.fail("metadata createdDate is not correct");
                return;
              }
            }
            //delete the object by id
            client.delete(port, "localhost", endpoint + "/" + id, res3 -> {
              assertStatus(context, res3, 204);
              future.complete();
            })
              .putHeader("X-Okapi-Tenant", "diku")
              .putHeader("Content-Type", "application/json")
              .putHeader("Accept", "application/json,text/plain")
              .exceptionHandler(e -> {
                future.fail(e);
              })
              .end();
          });
        })
          .putHeader("X-Okapi-Tenant", "diku")
          .putHeader("Content-Type", "application/json")
          .putHeader("Accept", "application/json,text/plain")
          .exceptionHandler(e -> {
            future.fail(e);
          })
          .end();

      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-Type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .putHeader("X-Okapi-Token", fakeJWT)
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end(ob.encode());
    return future;
  }

  private Future<Void> getGroupByInvalidUuid(TestContext context) {
    System.out.println("Retrieving a group by invalid uuid\n");
    Future<Void> future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/groups/q", res -> {
      assertStatus(context, res, 404);
      future.complete();
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "application/json")
      .exceptionHandler(e -> {
        future.fail(e);
      })
      .end();
    return future;
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
    Future<Void> f1 = Future.future();
    getEmptyUsers(context).setHandler(f1.completer());
    startFuture = f1
      .compose(v -> postUser(context))
      .compose(v -> deleteUser(context))
      .compose(v -> postUser(context))
      .compose(v -> getUser(context))
      .compose(v -> getUserByCQL(context))
      .compose(v -> getUserByCqlById(context))
      .compose(v -> getUserByInvalidCQL(context))
      .compose(v -> deleteNonExistingUser(context))
      .compose(v -> postAnotherUser(context))
      .compose(v -> getUsersByCQL(context, "id==x") /* empty result */)
      .compose(v -> getUsersByCQL(context, "id==\"\"", "bobcircle", "joeblock"))
      .compose(v -> getUsersByCQL(context, jSearch, "joeblock"))
      .compose(v -> putUserGood(context))
      .compose(v -> putUserBadUsername(context))
      .compose(v -> getGoodUser(context))
      .compose(v -> putUserBadId(context))
      .compose(v -> putUserWithNumericName(context))
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

  @Test
  public void test2Group(TestContext context) throws Exception {
    String url = "http://localhost:" + port + "/groups";
    String userUrl = "http://localhost:" + port + "/users";

    /**
     * add a group
     */
    CompletableFuture<Response> addGroupCF = new CompletableFuture();
    String addGroupURL = url;
    send(addGroupURL, context, HttpMethod.POST, fooGroupData,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addGroupCF));
    Response addGroupResponse = addGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(addGroupResponse.code, HttpURLConnection.HTTP_CREATED);
    groupID1 = addGroupResponse.body.getString("id");
    System.out.println(addGroupResponse.body
      + "\nStatus - " + addGroupResponse.code + " at " + System.currentTimeMillis() + " for " + addGroupURL);

    /**
     * update a group
     */
    CompletableFuture<Response> updateGroupCF = new CompletableFuture();
    String updateGroupURL = url + "/" + groupID1;
    send(updateGroupURL, context, HttpMethod.PUT, barGroupData,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(updateGroupCF));
    Response updateGroupResponse = updateGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(updateGroupResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
    System.out.println(updateGroupResponse.body
      + "\nStatus - " + updateGroupResponse.code + " at " + System.currentTimeMillis() + " for " + updateGroupURL);

    /**
     * delete a group
     */
    CompletableFuture<Response> deleteCleanCF = new CompletableFuture();
    String deleteCleanURL = url + "/" + groupID1;
    send(deleteCleanURL, context, HttpMethod.DELETE, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(deleteCleanCF));
    Response deleteCleanResponse = deleteCleanCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(deleteCleanResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
    System.out.println(deleteCleanResponse.body
      + "\nStatus - " + deleteCleanResponse.code + " at " + System.currentTimeMillis() + " for " + deleteCleanURL);

    /**
     * re-add a group
     */
    CompletableFuture<Response> addNewGroupCF = new CompletableFuture();
    send(addGroupURL, context, HttpMethod.POST, fooGroupData,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addNewGroupCF));
    Response addNewGroupResponse = addNewGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(addNewGroupResponse.code, HttpURLConnection.HTTP_CREATED);
    groupID1 = addNewGroupResponse.body.getString("id");
    System.out.println(addNewGroupResponse.body
      + "\nStatus - " + addNewGroupResponse.code + " at " + System.currentTimeMillis() + " for " + addGroupURL);

    /**
     * add a user
     */
    CompletableFuture<Response> addUserCF = new CompletableFuture();
    String addUserURL = userUrl;
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley", groupID1).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addUserCF));
    Response addUserResponse = addUserCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse.code, HttpURLConnection.HTTP_CREATED);
    String userID = addUserResponse.body.getString("id");
    System.out.println(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /**
     * add the same user name again
     */
    CompletableFuture<Response> addUserCF2 = new CompletableFuture();
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley", groupID1).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addUserCF2));
    Response addUserResponse2 = addUserCF2.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse2.code, 422);
    System.out.println(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /**
     * add the same user again with same id
     */
    CompletableFuture<Response> addUserCF3 = new CompletableFuture();
    send(addUserURL, context, HttpMethod.POST, createUser(userID, "jhandley", groupID1).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addUserCF3));
    Response addUserResponse3 = addUserCF3.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse3.code, 422);
    System.out.println(addUserResponse3.body
      + "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /**
     * add a user again with non existent patron group
     */
    CompletableFuture<Response> addUserCF4 = new CompletableFuture();
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(addUserCF4));
    Response addUserResponse4 = addUserCF4.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse4.code, 400);
    System.out.println(addUserResponse4.body
      + "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /**
     * add a user again with invalid uuid
     */
    CompletableFuture<Response> addUserCF4a = new CompletableFuture();
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley2nd", "invalid-uuid").encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(addUserCF4a));
    Response addUserResponse4a = addUserCF4a.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse4a.code, 400);
    System.out.println(addUserResponse4a.body
      + "\nStatus - " + addUserResponse4a.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /**
     * update a user again with non existent patron group
     */
    CompletableFuture<Response> updateUserCF = new CompletableFuture();
    send(addUserURL + "/" + userID, context, HttpMethod.PUT, createUser(userID, "jhandley2nd", "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(updateUserCF));
    Response updateUserResponse = updateUserCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(updateUserResponse.code, 400);
    System.out.println(updateUserResponse.body
      + "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /**
     * update a user again with existent patron group
     */
    CompletableFuture<Response> updateUser2CF = new CompletableFuture();
    send(addUserURL + "/" + userID, context, HttpMethod.PUT, createUser(userID, "jhandley2nd", groupID1).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(updateUser2CF));
    Response updateUser2Response = updateUser2CF.get(5, TimeUnit.SECONDS);
    context.assertEquals(updateUser2Response.code, 204);
    System.out.println(updateUser2Response.body
      + "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /**
     * get all users belonging to a specific group
     */
    CompletableFuture<Response> getUsersInGroupCF = new CompletableFuture();
    String getUsersInGroupURL = userUrl + "?query=patronGroup==" + groupID1;
    send(getUsersInGroupURL, context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF,
      200, new HTTPResponseHandler(getUsersInGroupCF));
    Response getUsersInGroupResponse = getUsersInGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(getUsersInGroupResponse.code, HttpURLConnection.HTTP_OK);
    System.out.println(getUsersInGroupResponse.body
      + "\nStatus - " + getUsersInGroupResponse.code + " at " + System.currentTimeMillis() + " for "
      + getUsersInGroupURL);
    context.assertTrue(isSizeMatch(getUsersInGroupResponse, 1));

    /**
     * get all groups in groups table
     */
    CompletableFuture<Response> getAllGroupCF = new CompletableFuture();
    String getAllGroupURL = url;
    send(getAllGroupURL, context, HttpMethod.GET, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, new HTTPResponseHandler(getAllGroupCF));
    Response getAllGroupResponse = getAllGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(getAllGroupResponse.code, HttpURLConnection.HTTP_OK);
    System.out.println(getAllGroupResponse.body
      + "\nStatus - " + getAllGroupResponse.code + " at " + System.currentTimeMillis() + " for "
      + getAllGroupURL);
    context.assertTrue(isSizeMatch(getAllGroupResponse, 5)); // 4 in reference + 1 in test

    /**
     * try to get via cql
     */
    CompletableFuture<Response> cqlCF = new CompletableFuture();
    String cqlURL = url + "?query=group==librarianFOO";
    send(cqlURL,
      context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
      new HTTPResponseHandler(cqlCF));
    Response cqlResponse = cqlCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(cqlResponse.code, HttpURLConnection.HTTP_OK);
    System.out.println(cqlResponse.body
      + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
    context.assertTrue(isSizeMatch(cqlResponse, 1));

    /**
     * delete a group - should fail as there is a user associated with the group
     */
    CompletableFuture<Response> delete1CF = new CompletableFuture();
    String delete1URL = url + "/" + groupID1;
    send(delete1URL, context, HttpMethod.DELETE, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(delete1CF));
    Response delete1Response = delete1CF.get(5, TimeUnit.SECONDS);
    context.assertEquals(delete1Response.code, HttpURLConnection.HTTP_BAD_REQUEST);
    System.out.println(delete1Response.body
      + "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

    /**
     * delete a nonexistent group - should return 404
     */
    CompletableFuture<Response> deleteNEGCF = new CompletableFuture();
    String deleteNEGURL = url + "/a492ffd2-b848-48bf-b716-1a645822279e";
    send(deleteNEGURL, context, HttpMethod.DELETE, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 404, new HTTPNoBodyResponseHandler(deleteNEGCF));
    Response deleteNEGResponse = deleteNEGCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(deleteNEGResponse.code, HttpURLConnection.HTTP_NOT_FOUND);
    System.out.println(deleteNEGResponse.body
      + "\nStatus - " + deleteNEGResponse.code + " at " + System.currentTimeMillis() + " for " + deleteNEGURL);

    /**
     * try to add a duplicate group
     */
    CompletableFuture<Response> dupCF = new CompletableFuture();
    send(url, context, HttpMethod.POST, fooGroupData,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPResponseHandler(dupCF));
    Response dupResponse = dupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(dupResponse.code, 422);
    System.out.println(dupResponse.body
      + "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + url);

    /**
     * get a group
     */
    CompletableFuture<Response> getSpecGroupCF = new CompletableFuture();
    String getSpecGroupURL = url + "/" + groupID1;
    send(getSpecGroupURL, context, HttpMethod.GET, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, new HTTPResponseHandler(getSpecGroupCF));
    Response getSpecGroupResponse = getSpecGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(getSpecGroupResponse.code, HttpURLConnection.HTTP_OK);
    System.out.println(getSpecGroupResponse.body
      + "\nStatus - " + getSpecGroupResponse.code + " at " + System.currentTimeMillis() + " for " + getSpecGroupURL);
    context.assertTrue("librarianFOO".equals(getSpecGroupResponse.body.getString("group")));

    /**
     * get a group bad id
     */
    CompletableFuture<Response> getBadIDCF = new CompletableFuture();
    String getBadIDURL = url + "/3748ec8d-8dbc-4717-819d-87c839e6905e";
    send(getBadIDURL, context, HttpMethod.GET, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 404, new HTTPNoBodyResponseHandler(getBadIDCF));
    Response getBadIDResponse = getBadIDCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(getBadIDResponse.code, HttpURLConnection.HTTP_NOT_FOUND);
    System.out.println(getBadIDResponse.body
      + "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

    /**
     * delete a group with users should fail
     */
    CompletableFuture<Response> deleteCF = new CompletableFuture();
    String delete = url + "/" + groupID1;
    send(delete, context, HttpMethod.DELETE, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(deleteCF));
    Response deleteResponse = deleteCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(deleteResponse.code, HttpURLConnection.HTTP_BAD_REQUEST);
    System.out.println(deleteResponse.body
      + "\nStatus - " + deleteResponse.code + " at " + System.currentTimeMillis() + " for " + delete);

    /* Create a user with a past-due expiration date */
    UUID expiredUserId = UUID.randomUUID();
    {
      Date now = new Date();
      Date pastDate = new Date(now.getTime() - (10 * 24 * 60 * 60 * 1000));
      String dateString = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'").format(pastDate);
      JsonObject expiredUserJson = new JsonObject()
        .put("id", expiredUserId.toString())
        .put("username", "bmoses")
        .put("patronGroup", groupID1)
        .put("active", true)
        .put("expirationDate", dateString)
        .put("personal", new JsonObject()
          .put("lastName", "Brown")
          .put("firstName", "Moses")
        );
      CompletableFuture<Response> addExpiredUserCF = new CompletableFuture();
      send(addUserURL, context, HttpMethod.POST, expiredUserJson.encode(),
        SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
        new HTTPResponseHandler(addExpiredUserCF));
      Response addExpiredUserResponse = addExpiredUserCF.get(5, TimeUnit.SECONDS);
      System.out.println(addExpiredUserResponse.body
        + "\nStatus - " + addExpiredUserResponse.code + " at "
        + System.currentTimeMillis() + " for " + addUserURL + " (addExpiredUser)");
      context.assertEquals(addExpiredUserResponse.code, 201);
      CompletableFuture<Void> getExpirationCF = new CompletableFuture();
      ExpirationTool.doExpirationForTenant(vertx, vertxContext, "diku").setHandler(res -> {
        getExpirationCF.complete(null);
      });
      getExpirationCF.get(5, TimeUnit.SECONDS);
      //TimeUnit.SECONDS.sleep(15);
      CompletableFuture<Response> getExpiredUserCF = new CompletableFuture();
      send(addUserURL + "/" + expiredUserId.toString(), context, HttpMethod.GET, null,
        SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
        new HTTPResponseHandler(getExpiredUserCF));
      Response getExpiredUserResponse = getExpiredUserCF.get(5, TimeUnit.SECONDS);
      context.assertEquals(getExpiredUserResponse.body.getBoolean("active"), false);
    }
  }

  @Test
  public void test3CrossTableQueries(TestContext context) throws Exception {
    String url = "http://localhost:" + port + "/users?query=";
    String userUrl = "http://localhost:" + port + "/users";

    CompletableFuture<Response> postGroupCF = new CompletableFuture();
    String postGroupURL = "http://localhost:" + port + "/groups";
    send(postGroupURL, context, HttpMethod.POST, barGroupData, SUPPORTED_CONTENT_TYPE_JSON_DEF,
      201, new HTTPResponseHandler(postGroupCF));
    Response postGroupResponse = postGroupCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(postGroupResponse.code, HttpURLConnection.HTTP_CREATED);
    String barGroupId = postGroupResponse.body.getString("id");
    context.assertNotNull(barGroupId);

    int inc = 0;
    CompletableFuture<Response> addUserCF = new CompletableFuture();
    String addUserURL = userUrl;
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addUserCF));
    Response addUserResponse = addUserCF.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse.code, HttpURLConnection.HTTP_CREATED);
    System.out.println(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    CompletableFuture<Response> addUserCF2 = new CompletableFuture();
    send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, new HTTPResponseHandler(addUserCF2));
    Response addUserResponse2 = addUserCF2.get(5, TimeUnit.SECONDS);
    context.assertEquals(addUserResponse2.code, HttpURLConnection.HTTP_CREATED);
    System.out.println(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    //query on users and sort by groups
    String url0 = userUrl;
    String url1 = url + StringUtil.urlEncode("cql.allRecords=1 sortBy patronGroup.group/sort.descending");
    //String url1 = userUrl;
    String url2 = url + StringUtil.urlEncode("cql.allrecords=1 sortBy patronGroup.group/sort.ascending");
    //query and sort on groups via users endpoint
    String url3 = url + StringUtil.urlEncode("patronGroup.group=lib* sortBy patronGroup.group/sort.descending");
    //query on users sort on users and groups
    String url4 = url + StringUtil.urlEncode("cql.allrecords=1 sortby patronGroup.group personal.lastName personal.firstName");
    //query on users and groups sort by groups
    String url5 = url + StringUtil.urlEncode("username=jhandley2nd and patronGroup.group=lib* sortby patronGroup.group");
    //query on users and sort by users
    String url6 = url + StringUtil.urlEncode("active=true sortBy username", "UTF-8");
    //non existant group - should be 0 results
    String url7 = url + StringUtil.urlEncode("username=jhandley2nd and patronGroup.group=abc* sortby patronGroup.group");
    //query by tag, should get one record
    String url8 = url + StringUtil.urlEncode("tags=foo");

    CompletableFuture<Response> cqlCF0 = new CompletableFuture();
    CompletableFuture<Response> cqlCF1 = new CompletableFuture();
    CompletableFuture<Response> cqlCF2 = new CompletableFuture();
    CompletableFuture<Response> cqlCF3 = new CompletableFuture();
    CompletableFuture<Response> cqlCF4 = new CompletableFuture();
    CompletableFuture<Response> cqlCF5 = new CompletableFuture();
    CompletableFuture<Response> cqlCF6 = new CompletableFuture();
    CompletableFuture<Response> cqlCF7 = new CompletableFuture();
    CompletableFuture<Response> cqlCF8 = new CompletableFuture();

    String[] urls = new String[]{url0, url1, url2, url3, url4, url5, url6, url7, url8};
    CompletableFuture<Response>[] cqlCF = new CompletableFuture[]{cqlCF0, cqlCF1, cqlCF2, cqlCF3, cqlCF4, cqlCF5, cqlCF6, cqlCF7, cqlCF8};

    for (int i = 0; i < 9; i++) {
      CompletableFuture<Response> cf = cqlCF[i];
      String cqlURL = urls[i];
      send(cqlURL, context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
        new HTTPResponseHandler(cf));
      Response cqlResponse = cf.get(5, TimeUnit.SECONDS);
      context.assertEquals(cqlResponse.code, HttpURLConnection.HTTP_OK);
      System.out.println(cqlResponse.body
        + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL + " (url" + (i) + ") : " + cqlResponse.body.toString());
      //requests should usually have 3 or 4 results
      switch (i) {
        case 8:
          context.assertEquals(1, cqlResponse.body.getInteger("totalRecords"));
          break;
        case 7:
          context.assertEquals(0, cqlResponse.body.getInteger("totalRecords"));
          break;
        case 6:
          context.assertTrue(cqlResponse.body.getInteger("totalRecords") > 2);
          break;
        case 5:
          context.assertEquals(1, cqlResponse.body.getInteger("totalRecords"));
          break;
        case 1:
          context.assertTrue(cqlResponse.body.getInteger("totalRecords") > 1);
          context.assertEquals("jhandley2nd", cqlResponse.body.getJsonArray("users").getJsonObject(0).getString("username"));
          break;
        case 2:
          context.assertNotEquals("jhandley2nd", cqlResponse.body.getJsonArray("users").getJsonObject(0).getString("username"));
          break;
        case 4:
          context.assertTrue(((String) (new JsonPathParser(cqlResponse.body).getValueAt("users[0].personal.lastName"))).startsWith("Triangle"));
          break;
        case 0:
          //Baseline test
          int totalRecords = cqlResponse.body.getInteger("totalRecords");
          context.assertTrue(totalRecords > 3, "totalRecords = " + totalRecords + " > 3");
          break;
        default:
          context.assertInRange(2, cqlResponse.body.getInteger("totalRecords"), 2);
          break;
      }
    }
  }

  private void send(String url, TestContext context, HttpMethod method, String content,
    String contentType, int errorCode, Handler<HttpClientResponse> handler) {
    HttpClient client = vertx.createHttpClient();
    HttpClientRequest request;
    if (content == null) {
      content = "";
    }
    Buffer buffer = Buffer.buffer(content);

    if (method == HttpMethod.POST) {
      request = client.postAbs(url);
    } else if (method == HttpMethod.DELETE) {
      request = client.deleteAbs(url);
    } else if (method == HttpMethod.GET) {
      request = client.getAbs(url);
    } else {
      request = client.putAbs(url);
    }
    request.exceptionHandler(error -> {
      context.fail(error.getMessage());
    })
      .handler(handler);
    //request.putHeader("Authorization", "diku");
    request.putHeader("x-okapi-tenant", "diku");
    request.putHeader("Accept", "application/json,text/plain");
    request.putHeader("Content-type", contentType);
    request.end(buffer);
  }

  class HTTPResponseHandler implements Handler<HttpClientResponse> {

    CompletableFuture<Response> event;

    public HTTPResponseHandler(CompletableFuture<Response> cf) {
      event = cf;
    }

    @Override
    public void handle(HttpClientResponse hcr) {
      hcr.bodyHandler(bh -> {
        Response r = new Response();
        r.code = hcr.statusCode();
        r.body = bh.toJsonObject();
        event.complete(r);
      });
    }
  }

  class HTTPNoBodyResponseHandler implements Handler<HttpClientResponse> {

    CompletableFuture<Response> event;

    public HTTPNoBodyResponseHandler(CompletableFuture<Response> cf) {
      event = cf;
    }

    @Override
    public void handle(HttpClientResponse hcr) {
      Response r = new Response();
      r.code = hcr.statusCode();
      event.complete(r);
    }
  }

  class Response {

    int code;
    JsonObject body;
  }

  private boolean isSizeMatch(Response r, int size) {
    if (r.body.getInteger("totalRecords") == size) {
      return true;
    }
    return false;
  }

  private static int userInc = 0;

  private static JsonObject createUser(String id, String name, String pgId) {
    userInc++;
    JsonObject user = new JsonObject();
    if (id != null) {
      user.put("id", id);
    } else {
      id = UUID.randomUUID().toString();
      user.put("id", id);
    }
    user.put("username", name);
    user.put("patronGroup", pgId);
    user.put("active", true);
    user.put("personal", new JsonObject()
      .put("lastName", "Triangle" + userInc)
      .put("firstName", "Jack" + userInc)
    );
    return user;
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
