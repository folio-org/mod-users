package org.folio.moduserstest;

import static io.vertx.core.json.Json.encode;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import static org.folio.moduserstest.RestITSupport.assertStatus;
import static org.folio.moduserstest.RestITSupport.delete;
import static org.folio.moduserstest.RestITSupport.deleteWithNoContentStatus;
import static org.folio.moduserstest.RestITSupport.get;
import static org.folio.moduserstest.RestITSupport.getJson;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.postWithOkStatus;
import static org.folio.moduserstest.RestITSupport.put;
import static org.folio.moduserstest.RestITSupport.putWithNoContentStatus;
import static org.folio.util.StringUtil.urlEncode;

import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
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
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestVerticleIT {

  private static final Logger log = LoggerFactory.getLogger(RestVerticleIT.class);

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String bobCircleId = "54afd8b8-fb3b-4de8-9b7c-299904887f7d";
  private static final String jackTriangleId = "e133841d-b645-4488-9e52-9762d560b617";
  private static final String annaRhombusId = "e8090974-8876-4411-befa-8ddcffad0b35";
  private static final String user777777Id = "72bd29f7-bf29-48bb-8259-d5ce78378a56";
  private static final String userIdWithWhitespace = "56bd29f7-bf29-48bb-8259-d5ce76378a42";

  private static final String FAKE_TOKEN = makeFakeJWT("bubba", UUID.randomUUID().toString(), "diku");
  private static final int DEFAULT_LIMIT = 10;

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
      new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").format(new Date()))
    .put("requestForSponsor", "Yes")
    .put("notificationsTo", "Proxy")
    .put("accrueTo", "Sponsor");


  @Rule
  public Timeout rule = Timeout.seconds(20);

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

  private Future<Void> getEmptyUsers(TestContext context) {
    log.info("Getting an empty user set\n");

    Future<HttpResponse<Buffer>> future = get("/users");

    return future.map(response -> {
      assertStatus(context, response, 200);

      JsonObject userCollectionObject = response.bodyAsJsonObject();
      if (userCollectionObject.getJsonArray("users").size() != 0
        || userCollectionObject.getInteger("totalRecords") != 0) {
        fail("Invalid return JSON: " + response.bodyAsString());
      }
      return null;
    });
  }

  private static void addTags(JsonObject u) {
    JsonArray tagList = new JsonArray();
    tagList.add("foo-tag");
    tagList.add("bar-tag");
    JsonObject tagobj = new JsonObject();
    tagobj.put("tagList", tagList);
    u.put("tags", tagobj);
  }

  private Future<Void> postUser(boolean withUserName) {
    log.info("Creating a new user\n");

    JsonObject user = new JsonObject()
      .put("id", joeBlockId)
      .put("active", true);
    if (withUserName) {
      user.put("username", "joeblock");
    }
    addTags(user);

    return postWithOkStatus(joeBlockId, "/users", user.encode());
  }

  private Future<Void> deleteNonExistingUser(TestContext context) {
    log.info("Deleting non-existing user\n");

    Future<HttpResponse<Buffer>> future = delete("/users/85936906-4737-4da7-b0fb-e8da080b97d8");

    return future.map(response -> {
      assertStatus(context, response, 404);
      return null;
    });
  }

  private Future<Void> deleteUser(TestContext context, String userId) {
    log.info("Deleting existing user\n");
    return deleteWithNoContentStatus(context, "/users/" + userId);
  }

  private Future<Void> postUserWithNumericName(TestContext context) {
    log.info("Creating a user with a numeric name\n");

    JsonObject user = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", true);

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }

  private Future<Void> getUser(TestContext context) {
    log.info("Retrieving a user\n");

    Future<JsonObject> future = getJson(context, "/users/" + joeBlockId);

    return future.map(user -> {
      if (user.getString("username").equals("joeblock")) {
        JsonObject tags = user.getJsonObject("tags");

        if (tags == null || !tags.encode().equals("{\"tagList\":[\"foo-tag\",\"bar-tag\"]}")) {
          fail("Bad value for tag list");
        }
        else {
          Date createdDate = null;
          try {
            createdDate = new DateTime(user.getString("createdDate")).toDate();
          } catch (Exception e) {
            fail(e.getMessage());
          }
          Date now = new Date();
          if (createdDate.after(now)) {
            fail("Bad value for createdDate");
          }
        }
      } else {
        fail("Unable to read proper data from JSON return value: " + encode(user));
      }
      return null;
    });

  }

  private Future<Void> getUserByCQL(TestContext context) {
    log.info("Getting user via CQL, by username\n");

    Future<JsonObject> future = getJson(context, "/users?query=" + urlEncode("username==joeblock"));

    return future.map(users -> {
      int totalRecords = users.getInteger("totalRecords");
      if (totalRecords != 1) {
        fail("Expected 1 record, got " + totalRecords);
      }

      JsonArray userList = users.getJsonArray("users");
      JsonObject user = userList.getJsonObject(0);
      if (!user.getString("username").equals("joeblock")) {
        fail("Unable to read proper data from JSON return value: " + encode(users));
      }

      return null;
    });
  }

  private Future<Void> getUserByCqlById(TestContext context) {
    log.info("Getting user via CQL, by user id\n");

    Future<JsonObject> future = getJson(context, "/users?query=" + urlEncode("(id==" + joeBlockId + ")"));

    return future.map(users -> {
      int totalRecords = users.getInteger("totalRecords");
      assertThat(totalRecords, is(1));

      JsonArray userList = users.getJsonArray("users");
      JsonObject user = userList.getJsonObject(0);
      assertThat("username of " + encode(user), user.getString("username"), is("joeblock"));

      return null;
    });
  }

  private Future<Void> getUserByInvalidCQL(TestContext context) {
    log.info("Getting user via invalid CQL\n");

    // empty CQL query triggers parse exception
    Future<HttpResponse<Buffer>> future = get("/users?query=");

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> postAnotherUser(TestContext context) {
    log.info("Creating another user\n");

    JsonObject user = new JsonObject()
      .put("username", "bobcircle")
      .put("id", bobCircleId)
      .put("active", true);

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      assertStatus(context, response,  201);
      return null;
    });
  }

  private Future<Void> getUsersByCQL(TestContext context, String cql, int limit, String... expectedUsernames) {
    log.info("Query users via CQL\n");

    Future<JsonObject> future = getJson(context, "/users?query=" + urlEncode(cql) + "&limit=" + limit);

    return future.map(json -> {

      int totalRecords = json.getInteger("totalRecords");
      JsonArray userList = json.getJsonArray("users");
      if (userList.size() != totalRecords) {
        fail("totalRecords=" + totalRecords + " mismatch users list: " + userList.encodePrettily());
      }

      List<String> usernames = new ArrayList<>();
      for (int i = 0; i < userList.size(); i++) {
        usernames.add(userList.getJsonObject(i).getString("username"));
      }
      assertThat(usernames, containsInAnyOrder(expectedUsernames));

      return null;
    });
  }

  private Future<Void> putUserGood(TestContext context, String id, boolean withUserName) {
    log.info("Making a valid user modification\n");

    JsonObject user = new JsonObject()
      .put("id", id)
      .put("active", false);
    if (withUserName) {
      user.put("username", "bobcircle");
    }

    return putWithNoContentStatus(context, id, "/users/" + id, encode(user));
  }

  private Future<Void> getGoodUser(TestContext context) {
    log.info("Getting the modified user\n");

    Future<JsonObject> future = getJson(context, "/users/" + bobCircleId);

    return future.map(user -> {
      if (user.getString("username").equals("bobcircle")) {
        Date createdDate = null;
        Date updatedDate = null;
        try {
          createdDate = new DateTime(user.getString("createdDate")).toDate();
          updatedDate = new DateTime(user.getString("updatedDate")).toDate();
        } catch (Exception e) {
          fail(e.getMessage());
        }

        Date now = new Date();
        if (createdDate.after(now) || updatedDate.after(now) || createdDate.after(updatedDate)) {
          fail("Bad value for createdDate and/or updatedDate");
        }
      } else {
        fail("Unable to read proper data from JSON return value: " + encode(user));
      }
      return null;
    });
  }

  private Future<Void> putUserBadUsername(TestContext context) {
    log.info("Trying to assign an invalid username \n");

    JsonObject user = new JsonObject()
      .put("username", "joeblock")
      .put("id", bobCircleId)
      .put("active", false);

    Future<HttpResponse<Buffer>> future = put("/users/" + bobCircleId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> putUserWithoutIdInMetadata(TestContext context) {
    log.info("Changing a user without id in metadata\n");

    JsonObject user = new JsonObject()
      .put("username", "bobcircle")
      .put("id", bobCircleId)
      .put("active", false)
      // metadata with createdDate but without createdByUserId
      // https://issues.folio.org/browse/RMB-459
      // https://issues.folio.org/browse/UIU-1069
      .put("metadata", new JsonObject().put("createdDate", "2000-12-31T01:02:03"));

    Future<HttpResponse<Buffer>> future = put("/users/" + bobCircleId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 204);
      return null;
    });
  }

  private Future<Void> putUserBadId(TestContext context) {
    log.info("Trying to assign an invalid id \n");

    JsonObject user = new JsonObject()
      .put("username", "bobcircle")
      .put("id", joeBlockId)
      .put("active", false);

    Future<HttpResponse<Buffer>> future = put("/users/" + joeBlockId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> putUserNotMatchingId(TestContext context) {
    log.info("Trying to Update user id \n");

    JsonObject user = new JsonObject()
      .put("username", "joeblock")
      .put("id", bobCircleId)
      .put("active", false);

    Future<HttpResponse<Buffer>> future = put("/users/" + joeBlockId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> putUserDuplicatedAddressType(TestContext context) {
    log.info("Attempting to update a user with two of the same address types\n");

    String addressTypeId = "4716a236-22eb-472a-9f33-d3456c9cc9d5";
    JsonObject user = new JsonObject()
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

    Future<HttpResponse<Buffer>> future = put("/users/" + joeBlockId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> putUserInvalidAddressType(TestContext context) {
    log.info("Attempting to update a user with invalid address types\n");

    JsonObject user = new JsonObject()
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

    Future<HttpResponse<Buffer>> future = put("/users/" + joeBlockId, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  // https://issues.folio.org/browse/MODUSERS-90
  // https://issues.folio.org/browse/MODUSERS-108
  private Future<Void> putUserWithNumericName(TestContext context) {
    log.info("Changing a user with numeric name\n");

    JsonObject user = new JsonObject()
      .put("username", "777777")
      .put("id", user777777Id)
      .put("active", false);

    Future<HttpResponse<Buffer>> future = put("/users/" + user777777Id, encode(user));

    return future.map(response -> {
      assertStatus(context, response, 404);
      return null;
    });
  }

  private Future<Void> createAddressType(TestContext context) {
    log.info("Creating an address type\n");

    JsonObject addressType = new JsonObject()
      .put("addressType", "sweethome")
      .put("desc", "The patron's primary residence");

    Future<HttpResponse<Buffer>> future = post("/addresstypes", encode(addressType));

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }

  private Future<Void> createBadAddressType(TestContext context) {
    log.info("Creating a bad address type\n");

    JsonObject addressType = new JsonObject()
      .put("desc", "The patron's summer residence");

    Future<HttpResponse<Buffer>> future = post("/addresstypes", encode(addressType));

    return future.map(response -> {
      assertStatus(context, response, 422);
      return null;
    });
  }

  private Future<Void> getAddressTypeUpdateUser(TestContext context) {
    log.info("Getting the new addresstype, updating a user with it\n");

    Future<JsonObject> f1 = getJson(context, "/addresstypes?query=addressType=sweethome")
      .map(entries -> {
        JsonObject addressType = entries.getJsonArray("addressTypes").getJsonObject(0);

        if (!addressType.getString("addressType").equals("sweethome")) {
          fail("addressType is not 'sweethome' in return addresstype");
        }
        return addressType;
      });

    Future<JsonObject> f2 = f1.compose(addressType -> {
      JsonObject user = new JsonObject()
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

      return put("/users/" + bobCircleId, encode(user))
        .map(response -> {
          assertStatus(context, response, 204);
          return addressType;
        });
    });

    return f2.compose(addressType -> delete("/addresstypes/" + addressType.getString("id")))
      .map(response -> {
        assertStatus(context, response, 400);
        return null;
      });
  }

  private Future<Void> deleteAddressTypeSQLError(TestContext context) {
    log.info("Deleting address type SQL error\n");

    Future<HttpResponse<Buffer>> future = delete("/addresstypes/x%2F");

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> deleteAddressTypeCQLError(TestContext context) {
    log.info("Deleting address type CQL error\n");

    Future<HttpResponse<Buffer>> future = delete("/addresstypes/x=");

    return future.map(response -> {
      assertStatus(context, response, 500);
      return null;
    });
  }

  private Future<Void> createAndDeleteAddressType(TestContext context) {
    log.info("Creating and deleting an address type\n");

    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "hardwork")
      .put("desc", "The patron's work address");

    Future<JsonObject> f1 = post("/addresstypes", encode(addressTypeObject))
      .map(response -> {
        assertStatus(context, response, 201);
        return response.bodyAsJsonObject();
      });

    return f1.compose(at -> delete("/addresstypes/" + at.getString("id")))
      .map(o -> {
        assertStatus(context, o, 204);
        return null;
      });
  }

  private Future<Void> postUserWithDuplicateAddressType(TestContext context) {
    log.info("Attempting to create a user with two of the same address types");

    String addressTypeId = "4716a236-22eb-472a-9f33-d3456c9cc9d5";
    JsonObject user = new JsonObject()
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

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> postUserBadAddress(TestContext context) {
    log.info("Trying to create a bad address\n");

    String addressTypeId = "1b1ad9a7-5af5-4545-b5f0-4242ba5f62c8";
    JsonObject user = new JsonObject()
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

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      assertStatus(context, response, 400);
      return null;
    });
  }

  private Future<Void> postUserWithDuplicateId(TestContext context) {
    log.info("Attempting to create a user with duplicate id");

    String uuid = UUID.randomUUID().toString();
    JsonObject user1 = new JsonObject().put("id", uuid);
    JsonObject user2 = new JsonObject().put("id", uuid);

    // create test user one
    Future<Void> f1 = post("/users", encode(user1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    // fail attempting to create user with duplicate id
    return f1.compose(v -> post("/users", encode(user2)))
      .map(response -> {
        assertStatus(context, response, 422);

        JsonObject validationErrorRes = response.bodyAsJsonObject();
        JsonArray validationErrors = validationErrorRes.getJsonArray("errors");

        if (validationErrors.isEmpty()) {
          fail("Did not return expected validation errors");
        } else {
          String errorMessage = validationErrors.getJsonObject(0).getString("message");
          context.assertEquals(1, validationErrors.size());
          context.assertEquals(errorMessage, "User with this id already exists");
        }
        return null;
      });
  }

  private Future<Void> postUserWithDuplicateUsername(TestContext context) {
    log.info("Attempting to create a user with duplicate username");

    JsonObject user1 = new JsonObject()
      .put("username", "the_twin")
      .put("id",  UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("username", "the_twin")
      .put("id",  UUID.randomUUID().toString());

    // create test user one
    Future<Void> f1 = post("/users", encode(user1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    // creating a second user with the same username should fail
    return f1.compose(v -> post("/users", encode(user2)))
      .map(response -> {
        assertStatus(context, response, 422);

        JsonObject validationErrorRes = response.bodyAsJsonObject();
        JsonArray validationErrors = validationErrorRes.getJsonArray("errors");

        if (validationErrors.isEmpty()) {
          fail("Did not return expected validation errors");
        } else {
          String errorMessage = validationErrors.getJsonObject(0).getString("message");
          context.assertEquals(1, validationErrors.size());
          context.assertEquals(errorMessage, "User with this username already exists");
        }
        return null;
      });
  }

  private Future<Void> putUserWithDuplicateUsername(TestContext context) {
    log.info("Changing a user to username that already exists\n");

    JsonObject user1 = new JsonObject()
      .put("username", "left_shoe")
      .put("id", UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("username", "right_shoe")
      .put("id", UUID.randomUUID().toString());

    // create test user one
    Future<Void> f1 = post("/users", encode(user1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    Future<Void> f2 = f1.compose(v -> post("/users", encode(user2)))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    return f2.compose(v -> {
      // attempt to update user2 changing username to a duplicate
      user2.put("username", "left_shoe");

      return put("/users/" + user2.getString("id"), encode(user2))
        .map(response -> {
          assertStatus(context, response, 400);

          context.assertEquals("User with this username already exists", response.bodyAsString());

          return null;
        });
    });
  }

  // https://issues.folio.org/browse/MODUSERS-147
  private Future<Void> postTwoUsersWithoutUsername(TestContext context) {
    log.info("Attempting to create two users without username");

    JsonObject user1 = new JsonObject()
      .put("id",  UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("id",  UUID.randomUUID().toString());

    Future<Void> f1 = post("/users", encode(user1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    return f1.compose(v -> post("/users", encode(user2)))
      .map(response -> {
        // should succeed, there can be any number of users without username
        assertStatus(context, response, 201);
        return null;
      });
  }

  // https://issues.folio.org/browse/MODUSERS-147
  private Future<Void> putSecondUserWithoutUsername(TestContext context) {
    log.info("Updating second user to have no username");

    JsonObject user1 = new JsonObject()
      .put("id", UUID.randomUUID().toString());
    JsonObject user2 = new JsonObject()
      .put("username", "name_for_sale")
      .put("id", UUID.randomUUID().toString());

    Future<Void> f1 = post("/users", encode(user1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    Future<Void> f2 = f1.compose(v -> post("/users", encode(user2)))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    return f2.compose(v -> {
      user2.remove("username");  // try to PUT with username removed

      return put("/users/" + user2.getString("id"), encode(user2))
        .map(response -> {
          assertStatus(context, response, 204);
          return null;
        });
    });
  }

  // https://issues.folio.org/browse/MODUSERS-118
  private Future<Void> postUserWithDuplicateBarcode(TestContext context) {
    log.info("Attempting to create a user with duplicate barcode");

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

    Future<Void> f1 = post("/users", encode(userObject1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    return f1.compose(v -> post("/users", encode(userObject2)))
      // fail attempting to create user with duplicate barcode
      .map(response -> {
        assertStatus(context, response, 422);

        JsonObject validationErrorRes = response.bodyAsJsonObject();
        JsonArray validationErrors = validationErrorRes.getJsonArray("errors");
        if (validationErrors.isEmpty()) {
          fail("Did not return expected validation errors");
        } else {
          String errorMessage = validationErrors.getJsonObject(0).getString("message");
          assertThat(1, is(validationErrors.size()));
          assertThat(errorMessage, is("This barcode has already been taken"));
        }

        return null;
      });
  }

  // https://issues.folio.org/browse/MODUSERS-118
  private Future<Void> putUserWithDuplicateBarcode(TestContext context) {
    log.info("Changing a user to barcode that already exists\n");

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

    // create test user one
    Future<Void> f1 = post("/users", encode(userObject1))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    // create test user two
    Future<Void> f2 = f1.compose(v -> post("/users", encode(userObject2)))
      .map(response -> {
        assertStatus(context, response, 201);
        return null;
      });

    return f2.compose(v -> {
      // fail attempting to update user changing barcode to a duplicate
      userObject2.put("barcode", "304276530498752");

      return put("/users/" + testUserFourId, encode(userObject2))
        .map(response -> {
          assertStatus(context, response, 400);

          String errorMessage = response.bodyAsString();
          assertThat(errorMessage, is("This barcode has already been taken"));

          return null;
        });
    });
  }

  private Future<Void> createProxyfor(TestContext context) {
    log.info("Creating a new proxyfor entry\n");

    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    Future<HttpResponse<Buffer>> future = post("/proxiesfor", proxyObject.encode());

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }

  private Future<Void> createProxyforWithSameUserId(TestContext context) {
    log.info("Trying to create a proxyfor with an existing userid\n");

    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "5b0a9a0b-6eb6-447c-bc31-9c99940a29c5");

    Future<HttpResponse<Buffer>> future = post("/proxiesfor", proxyObject.encode());

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }

  private Future<Void> createProxyforWithSameProxyUserId(TestContext context) {
    log.info("Trying to create a proxyfor with an existing proxy userid\n");

    JsonObject proxyObject = new JsonObject()
      .put("userId", "bd2cbc13-9d43-4a74-8090-75bc4e26a8df")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    Future<HttpResponse<Buffer>> future = post("/proxiesfor", proxyObject.encode());

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }

  private Future<Void> failToCreateDuplicateProxyfor(TestContext context) {
    log.info("Trying to create a proxyfor entry with the same id and proxy user id\n");

    JsonObject proxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    Future<HttpResponse<Buffer>> future = post("/proxiesfor", proxyObject.encode());

    return future.map(response -> {
      assertStatus(context, response, 422);
      return null;
    });
  }

  private Future<Void> getProxyforCollection(TestContext context) {
    log.info("Getting proxyfor entries\n");

    Future<JsonObject> future = getJson(context, "/proxiesfor");

    return future.map(proxies -> {
      JsonArray proxyForArray = proxies.getJsonArray("proxiesFor");
      if (proxyForArray.size() != 3) {
        fail("Expected 3 entries, found " + proxyForArray.size());
      }
      return null;
    });
  }

  private Future<Void> findAndGetProxyfor(TestContext context) {
    log.info("Find and retrieve a particular proxyfor entry\n");

    log.info("Making CQL request\n");
    Future<String> proxyId = getProxyId(context,
      "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    log.info("Making get-by-id request\n");

    return proxyId.compose(id -> get("/proxiesfor/" + id))
      .map(response -> {
        assertStatus(context, response, 200);
        return null;
      });
  }

  private Future<Void> findAndUpdateProxyfor(TestContext context) {
    log.info("Find and update a particular proxyfor entry\n");

    log.info("Making CQL request\n");
    Future<String> proxyId = getProxyId(context,
      "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    JsonObject modifiedProxyObject = new JsonObject()
      .put("userId", "2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5")
      .put("proxyUserId", "2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    log.info("Making put-by-id request\n");
    return proxyId.compose(id -> put("/proxiesfor/" + id, encode(modifiedProxyObject)))
      .map(response -> {
        assertStatus(context, response, 204);
        return null;
      });
  }

  private Future<String> getProxyId(TestContext context, String requestUrl) {
    Future<JsonObject> resultJson = getJson(context, requestUrl);

    return resultJson.map(result -> {
      JsonArray proxyForArray = result.getJsonArray("proxiesFor");
      if (proxyForArray.size() != 1) {
        fail("Expected 1 entry, found " + proxyForArray.size());
      }

      JsonObject proxyForObject = proxyForArray.getJsonObject(0);
      return proxyForObject.getString("id");
    });
  }

  private Future<Void> findAndDeleteProxyfor(TestContext context) {
    log.info("Find and delete a particular proxyfor entry");

    log.info("Making CQL request\n");
    Future<String> proxyId = getProxyId(context,
      "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa");

    return proxyId.compose(id -> delete("/proxiesfor/" + id))
      .map(response -> {
        assertStatus(context, response, 204);
        return null;
      });
  }

  private Future<Void> createTestDeleteObjectById(TestContext context, JsonObject ob,
                                                  String endpoint, boolean checkMeta) {
    log.info(String.format(
      "Creating object %s at endpoint %s", ob.encode(), endpoint));

    Map<String, String> ah = new HashMap<>();
    ah.put("X-Okapi-Token", FAKE_TOKEN);

    Future<String> f1 = post(endpoint, encode(ob), ah)
      .map(response -> {
        assertStatus(context, response, 201);

        return response.bodyAsJsonObject().getString("id");
      });

    //Get the object by id
    Future<String> f2 = f1.compose(v -> getJson(context, endpoint + "/" + v))
      .map(response -> {
        if (checkMeta) {
          Date createdDate = null;
          try {
            JsonObject metadata = response.getJsonObject("metadata");
            if (metadata == null) {
              fail(String.format("No 'metadata' field in result: %s",
                response.toString()));
            }
            createdDate = new DateTime(metadata.getString("createdDate")).toDate();
          } catch (Exception e) {
            fail(e.getMessage());
          }

          Date now = new Date();
          if (!createdDate.before(now)) {
            fail("metadata createdDate is not correct");
          }
        }
        return response.getString("id");
      });

    //delete the object by id
    return f2.compose(id -> deleteWithNoContentStatus(context, endpoint + "/" + id));
  }

  private Future<Void> getGroupByInvalidUuid(TestContext context) {
    log.info("Retrieving a group by invalid uuid\n");

    Future<HttpResponse<Buffer>> future = get("/groups/q");

    return future.map(response -> {
      assertStatus(context, response, 404);
      return null;
    });
  }

  @Test
  public void test1Sequential(TestContext context) {
    /*
      The CQL used for searching when a single j has been entered into the
      search slot
     */
    final String jSearch = "(((username=\"j*\" or personal.firstName=\"j*\" or "
      + "personal.lastName=\"j*\" or personal.email=\"j*\" or barcode=\"j*\" or "
      + "id=\"j*\" or externalSystemId=\"j*\")) and active=\"true\") "
      + "sortby personal.lastName personal.firstName";

    Async async = context.async();
    Future<Void> startFuture;
    startFuture = getEmptyUsers(context)
      .compose(v -> postUser(false))
      .compose(v -> putUserGood(context, joeBlockId, false))
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> postUser(true))
      .compose(v -> deleteUser(context, joeBlockId))
      .compose(v -> postUser(true))
      .compose(v -> getUser(context))
      .compose(v -> getUserByCQL(context))
      .compose(v -> getUserByCqlById(context))
      .compose(v -> getUserByInvalidCQL(context))
      .compose(v -> deleteNonExistingUser(context))
      .compose(v -> postAnotherUser(context))
      .compose(v -> getUsersByCQL(context, "id==x", DEFAULT_LIMIT) /* empty result */)
      .compose(v -> getUsersByCQL(context, "id==\"\"", DEFAULT_LIMIT, "bobcircle", "joeblock"))
      .compose(v -> getUsersByCQL(context, jSearch, DEFAULT_LIMIT, "joeblock"))
      .compose(v -> putUserGood(context, bobCircleId, true))
      .compose(v -> putUserBadUsername(context))
      .compose(v -> putUserWithoutIdInMetadata(context))
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
      .compose(v -> getUsersByCQL(context, String.format("id==%s", userIdWithWhitespace), DEFAULT_LIMIT, "user name"))
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

    JsonObject user = new JsonObject()
      .put("username", " user name ")
      .put("id", userIdWithWhitespace)
      .put("active", true);

    Future<HttpResponse<Buffer>> future = post("/users", encode(user));

    return future.map(response -> {
      assertStatus(context, response, 201);
      return null;
    });
  }
}
