package org.folio.moduserstest;

import static io.vertx.core.json.Json.encode;
import static org.folio.moduserstest.RestITSupport.assertStatus;
import static org.folio.moduserstest.RestITSupport.get;
import static org.folio.moduserstest.RestITSupport.getJson;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.postWithOkStatus;
import static org.folio.moduserstest.RestITSupport.put;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.util.StringUtil.urlEncode;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RestVerticleIT {

  private static final Logger log = LogManager.getLogger(RestVerticleIT.class);

  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String bobCircleId = "54afd8b8-fb3b-4de8-9b7c-299904887f7d";
  private static final String annaRhombusId = "e8090974-8876-4411-befa-8ddcffad0b35";
  private static final String user777777Id = "72bd29f7-bf29-48bb-8259-d5ce78378a56";

  private static final int DEFAULT_LIMIT = 10;

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

  static Future<Void> putWithNoContentStatus(TestContext context, String body) {
    return RestITSupport.putWithNoContentStatus(context, joeBlockId,
      "/users/" + joeBlockId, body);
  }

  static Future<HttpResponse<Buffer>> delete(String request) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    RestITSupport.client.delete(RestITSupport.port, RestITSupport.LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("accept", "*/*")
      .send(promise);

    return promise.future();
  }

  private Future<Void> getEmptyUsers(TestContext context) {
    log.info("Getting an empty user set\n");

    Future<HttpResponse<Buffer>> future = get("/users");

    return future.map(response -> {
      assertStatus(context, response, 200);

      JsonObject userCollectionObject = response.bodyAsJsonObject();
      if (userCollectionObject.getJsonArray("users").size() != 0
        || userCollectionObject.getInteger("totalRecords") != 0) {
        fail("Expected users array to be empty and totalRecords = 0 but got: "
            + response.bodyAsString());
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

  private Future<Void> postUser() {
    log.info("Creating a new user\n");

    JsonObject user = new JsonObject()
      .put("id", joeBlockId)
      .put("active", false)
      .put("username", "joeblock");

    addTags(user);

    return postWithOkStatus(joeBlockId, "/users", user.encode());
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

  private Future<Void> getUsersByCQL(TestContext context, String cql, String... expectedUsernames) {
    log.info("Query users via CQL\n");

    Future<JsonObject> future = getJson(context, "/users?query=" + urlEncode(cql)
      + "&limit=" + RestVerticleIT.DEFAULT_LIMIT);

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

  private Future<Void> putUserGood(TestContext context) {
    log.info("Making a valid user modification\n");

    JsonObject user = new JsonObject()
      .put("id", joeBlockId)
      .put("active", true)
      .put("username", "joeblock");

    return putWithNoContentStatus(context,
      encode(user));
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
  private Future<Void> cannotReplaceUserThatDoesNotExist(TestContext context) {
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

  private Future<Void> deleteAddressTypeThatDoesNotExist(TestContext context) {
    log.info("Deleting address type that does not exist\n");

    Future<HttpResponse<Buffer>> future = delete("/addresstypes/foo");

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

  private Future<Void> cannotUpdateUserWithUnknownAddressType(TestContext context) {
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
    Future<String> proxyId = getProxyId(context
    );

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
    Future<String> proxyId = getProxyId(context
    );

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

  private Future<String> getProxyId(TestContext context) {
    Future<JsonObject> resultJson = getJson(context,
      "/proxiesfor?query=userId=2498aeb2-23ca-436a-87ea-a4e1bfaa5bb5+AND+proxyUserId=2062d0ef-3f3e-40c5-a870-5912554bc0fa");

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
    Future<String> proxyId = getProxyId(context
    );

    return proxyId.compose(id -> delete("/proxiesfor/" + id))
      .map(response -> {
        assertStatus(context, response, 204);
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

    Future<Void> startFuture = getEmptyUsers(context)
      .compose(v -> postUser())
      .compose(v -> putUserGood(context))
      .compose(v -> getUserByCQL(context))
      .compose(v -> getUserByCqlById(context))
      .compose(v -> getUserByInvalidCQL(context))
      .compose(v -> postAnotherUser(context))
      .compose(v -> getUsersByCQL(context, "id==x") /* empty result */)
      .compose(v -> getUsersByCQL(context, "id==\"\"", "bobcircle", "joeblock"))
      .compose(v -> getUsersByCQL(context, jSearch, "joeblock"))
      .compose(v -> putUserBadUsername(context))
      .compose(v -> putUserWithoutIdInMetadata(context))
      .compose(v -> putUserBadId(context))
      .compose(v -> putUserNotMatchingId(context))
      .compose(v -> putUserDuplicatedAddressType(context))
      .compose(v -> putUserInvalidAddressType(context))
      .compose(v -> cannotReplaceUserThatDoesNotExist(context))
      .compose(v -> putUserWithDuplicateUsername(context))
      .compose(v -> putUserWithDuplicateBarcode(context))
      .compose(v -> createAddressType(context))
      .compose(v -> createBadAddressType(context))
      .compose(v -> createAndDeleteAddressType(context))
      .compose(v -> deleteAddressTypeThatDoesNotExist(context))
      .compose(v -> deleteAddressTypeCQLError(context))
      .compose(v -> cannotUpdateUserWithUnknownAddressType(context))
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
      .compose(v -> findAndDeleteProxyfor(context));

    startFuture.onComplete(res -> {
      if (res.succeeded()) {
        async.complete();
      } else {
        res.cause().printStackTrace();
        context.fail(res.cause());
      }
    });
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
