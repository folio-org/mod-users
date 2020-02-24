package org.folio.moduserstest;

import static io.vertx.core.http.HttpMethod.DELETE;
import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;
import static io.vertx.core.http.HttpMethod.PUT;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.apache.commons.lang3.StringUtils.defaultString;

import static org.folio.moduserstest.RestITSupport.HTTP_LOCALHOST;
import static org.folio.moduserstest.RestITSupport.delete;
import static org.folio.moduserstest.RestITSupport.fail;
import static org.folio.moduserstest.RestITSupport.get;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.put;
import static org.folio.util.StringUtil.urlEncode;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
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
import org.folio.rest.tools.parser.JsonPathParser;
import org.folio.rest.utils.ExpirationTool;


@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class GroupIT {

  private final String userUrl = HTTP_LOCALHOST + RestITSupport.port() + "/users";
  private final String groupUrl = HTTP_LOCALHOST + RestITSupport.port() + "/groups";

  private static final String fooGroupData = "{\"group\": \"librarianFOO\",\"desc\": \"yet another basic lib group\"}";
  private static final String barGroupData = "{\"group\": \"librarianBAR\",\"desc\": \"and yet another basic lib group\"}";
  private static final String limitsUrl = "/patron-block-limits";

  private static final Logger log = LoggerFactory.getLogger(GroupIT.class);

  private static int userInc = 0;

  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) {
    RestITSupport.setUp();

    Async async = context.async();

    TenantClient tenantClient = new TenantClient(HTTP_LOCALHOST + RestITSupport.port(), "diku", "diku");
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
  public void deletingGroupDeletesItsPatronBlockLimits(TestContext context) throws Exception {
    /*
      create a new patron group
     */
    final String newGroupId = "1de95200-72e4-4967-bdf8-257fb7559888";

    JsonObject group = new JsonObject()
      .put("id", newGroupId)
      .put("group", "test_group")
      .put("desc", "group description");

    Response createGroupResponse =
      send(groupUrl, POST, group.encode(), HTTPResponseHandlers.json()).get(5, SECONDS);
    context.assertEquals(createGroupResponse.code, HTTP_CREATED);

    /*
      create 3 patron block limits: 2 for the newly created group, 1 for a different group
     */
    checkExistingLimitsByIds(context);

    String limitId1 = "1de95200-72e4-4967-bdf8-000000000001";
    String limitId2 = "1de95200-72e4-4967-bdf8-000000000002";
    String limitId3 = "1de95200-72e4-4967-bdf8-000000000003";

    String limitForNewGroup1 = new JsonObject()
      .put("id", limitId1)
      .put("patronGroupId", newGroupId)
      .put("conditionId", "3d7c52dc-c732-4223-8bf8-e5917801386f")
      .put("value", 10)
      .encodePrettily();

    String limitForNewGroup2 = new JsonObject()
      .put("id", limitId2)
      .put("patronGroupId", newGroupId)
      .put("conditionId", "72b67965-5b73-4840-bc0b-be8f3f6e047e")
      .put("value", 11)
      .encodePrettily();

    String limitForAnotherGroup = new JsonObject()
      .put("id", limitId3)
      .put("patronGroupId", "503a81cd-6c26-400f-b620-14c08943697c")
      .put("conditionId", "72b67965-5b73-4840-bc0b-be8f3f6e047e")
      .put("value", 12)
      .encodePrettily();

    CompletableFuture.completedFuture(null)
      .thenComposeAsync(r -> send(limitsUrl, POST, limitForNewGroup1, HTTPResponseHandlers.json()))
      .thenComposeAsync(r -> send(limitsUrl, POST, limitForNewGroup2, HTTPResponseHandlers.json()))
      .thenComposeAsync(r -> send(limitsUrl, POST, limitForAnotherGroup, HTTPResponseHandlers.json()))
      .get(5, SECONDS);

    checkExistingLimitsByIds(context, limitId1, limitId2, limitId3);

     /*
      delete the new group, make sure its limits are gone
     */
    String deleteGroupUrl = groupUrl + "/" + newGroupId;
    Response deleteGroupResponse =
      send(deleteGroupUrl, DELETE, null, HTTPResponseHandlers.empty()).get(5, SECONDS);
    context.assertEquals(deleteGroupResponse.code, HTTP_NO_CONTENT);

    checkExistingLimitsByIds(context, limitId3);
  }

  @Test
  public void test3CrossTableQueries(TestContext context) throws Exception {
    CompletableFuture<Response> postGroupCF = send(groupUrl, POST, barGroupData, HTTPResponseHandlers.json());
    Response postGroupResponse = postGroupCF.get(5, SECONDS);
    context.assertEquals(postGroupResponse.code, HTTP_CREATED);
    String barGroupId = postGroupResponse.body.getString("id");
    context.assertNotNull(barGroupId);

    int inc = 0;
    CompletableFuture<Response> addUserCF = send(userUrl, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse = addUserCF.get(5, SECONDS);
    context.assertEquals(addUserResponse.code, HTTP_CREATED);
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    CompletableFuture<Response> addUserCF2 = send(userUrl, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    context.assertEquals(addUserResponse2.code, HTTP_CREATED);
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    String url = HTTP_LOCALHOST + RestITSupport.port() + "/users?query=";

    //query on users and sort by groups
    String url0 = userUrl;
    String url1 = url + urlEncode("cql.allRecords=1 sortBy patronGroup.group/sort.descending");
    //String url1 = userUrl;
    String url2 = url + urlEncode("cql.allrecords=1 sortBy patronGroup.group/sort.ascending");
    //query and sort on groups via users endpoint
    String url3 = url + urlEncode("patronGroup.group=lib* sortBy patronGroup.group/sort.descending");
    //query on users sort on users and groups
    String url4 = url + urlEncode("cql.allrecords=1 sortby patronGroup.group personal.lastName personal.firstName");
    //query on users and groups sort by groups
    String url5 = url + urlEncode("username=jhandley2nd and patronGroup.group=lib* sortby patronGroup.group");
    //query on users and sort by users
    String url6 = url + urlEncode("active=true sortBy username", "UTF-8");
    //non existant group - should be 0 results
    String url7 = url + urlEncode("username=jhandley2nd and patronGroup.group=abc* sortby patronGroup.group");

    String[] urls = new String[]{url0, url1, url2, url3, url4, url5, url6, url7};

    for (int i = 0; i < 8; i++) {
      String cqlURL = urls[i];
      CompletableFuture<Response> cf = send(cqlURL, GET, null, HTTPResponseHandlers.json());

      Response cqlResponse = cf.get(5, SECONDS);
      context.assertEquals(cqlResponse.code, HTTP_OK);
      log.info(cqlResponse.body
        + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL + " (url" + (i) + ") : " + cqlResponse.body.toString());
      //requests should usually have 3 or 4 results
      switch (i) {
        case 5:
          context.assertEquals(1, cqlResponse.body.getInteger("totalRecords"));
          break;
        case 7:
          context.assertEquals(0, cqlResponse.body.getInteger("totalRecords"));
          break;
        case 6:
          context.assertTrue(cqlResponse.body.getInteger("totalRecords") > 2);
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

  @Test
  public void test2Group(TestContext context) throws Exception {
    /*
      add a group
     */
    CompletableFuture<Response> addGroupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response addGroupResponse = addGroupCF.get(5, SECONDS);
    context.assertEquals(addGroupResponse.code, HTTP_CREATED);
    String groupID1 = addGroupResponse.body.getString("id");
    log.info(addGroupResponse.body
      + "\nStatus - " + addGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      update a group
     */
    String updateGroupURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> updateGroupCF = send(updateGroupURL, PUT, barGroupData, HTTPResponseHandlers.empty());
    Response updateGroupResponse = updateGroupCF.get(5, SECONDS);
    context.assertEquals(updateGroupResponse.code, HTTP_NO_CONTENT);
    log.info(updateGroupResponse.body
      + "\nStatus - " + updateGroupResponse.code + " at " + System.currentTimeMillis() + " for " + updateGroupURL);

    /*
      delete a group
     */
    String deleteCleanURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> deleteCleanCF = send(deleteCleanURL, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteCleanResponse = deleteCleanCF.get(5, SECONDS);
    context.assertEquals(deleteCleanResponse.code, HTTP_NO_CONTENT);
    log.info(deleteCleanResponse.body
      + "\nStatus - " + deleteCleanResponse.code + " at " + System.currentTimeMillis() + " for " + deleteCleanURL);

    /*
      re-add a group
     */
    CompletableFuture<Response> addNewGroupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response addNewGroupResponse = addNewGroupCF.get(5, SECONDS);
    context.assertEquals(addNewGroupResponse.code, HTTP_CREATED);
    groupID1 = addNewGroupResponse.body.getString("id");
    log.info(addNewGroupResponse.body
      + "\nStatus - " + addNewGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      add a user
     */
    String addUserURL = userUrl;
    CompletableFuture<Response> addUserCF = send(addUserURL, POST, createUser(null, "jhandley", groupID1).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse = addUserCF.get(5, SECONDS);
    context.assertEquals(addUserResponse.code, HTTP_CREATED);
    String userID = addUserResponse.body.getString("id");
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user name again
     */
    CompletableFuture<Response> addUserCF2 = send(addUserURL, POST, createUser(null, "jhandley", groupID1).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    context.assertEquals(addUserResponse2.code, 422);
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user again with same id
     */
    CompletableFuture<Response> addUserCF3 = send(addUserURL, POST, createUser(userID, "jhandley", groupID1).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse3 = addUserCF3.get(5, SECONDS);
    context.assertEquals(addUserResponse3.code, 422);
    log.info(addUserResponse3.body
      + "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with non existent patron group
     */
    CompletableFuture<Response> addUserCF4 = send(addUserURL, POST,
      createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4 = addUserCF4.get(5, SECONDS);
    context.assertEquals(addUserResponse4.code, HTTP_BAD_REQUEST);
    log.info(addUserResponse4.body
      + "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with invalid uuid
     */
    CompletableFuture<Response> addUserCF4a = send(addUserURL, POST, createUser(null, "jhandley2nd", "invalid-uuid").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4a = addUserCF4a.get(5, SECONDS);
    context.assertEquals(addUserResponse4a.code, HTTP_BAD_REQUEST);
    log.info(addUserResponse4a.body
      + "\nStatus - " + addUserResponse4a.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      update a user again with non existent patron group
     */
    CompletableFuture<Response> updateUserCF = send(addUserURL + "/" + userID, PUT, createUser(userID, "jhandley2nd",
      "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(), HTTPResponseHandlers.empty());
    Response updateUserResponse = updateUserCF.get(5, SECONDS);
    context.assertEquals(updateUserResponse.code, HTTP_BAD_REQUEST);
    log.info(updateUserResponse.body
      + "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      update a user again with existent patron group
     */
    CompletableFuture<Response> updateUser2CF = send(addUserURL + "/" + userID, PUT,
      createUser(userID, "jhandley2nd", groupID1).encode(),
      HTTPResponseHandlers.empty());
    Response updateUser2Response = updateUser2CF.get(5, SECONDS);
    context.assertEquals(updateUser2Response.code, HTTP_NO_CONTENT);
    log.info(updateUser2Response.body
      + "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      get all users belonging to a specific group
     */
    String getUsersInGroupURL = userUrl + "?query=patronGroup==" + groupID1;
    CompletableFuture<Response> getUsersInGroupCF = send(getUsersInGroupURL, GET, null,
      HTTPResponseHandlers.json());
    Response getUsersInGroupResponse = getUsersInGroupCF.get(5, SECONDS);
    context.assertEquals(getUsersInGroupResponse.code, HTTP_OK);
    log.info(getUsersInGroupResponse.body
      + "\nStatus - " + getUsersInGroupResponse.code + " at " + System.currentTimeMillis() + " for "
      + getUsersInGroupURL);
    context.assertTrue(isSizeMatch(getUsersInGroupResponse, 1));

    /*
      get all groups in groups table
     */
    CompletableFuture<Response> getAllGroupCF = send(groupUrl, GET, null, HTTPResponseHandlers.json());
    Response getAllGroupResponse = getAllGroupCF.get(5, SECONDS);
    context.assertEquals(getAllGroupResponse.code, HTTP_OK);
    log.info(getAllGroupResponse.body
      + "\nStatus - " + getAllGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);
    context.assertTrue(isSizeMatch(getAllGroupResponse, 5)); // 4 in reference + 1 in test

    /*
      try to get via cql
     */
    String cqlURL = groupUrl + "?query=group==librarianFOO";
    CompletableFuture<Response> cqlCF = send(cqlURL, GET, null, HTTPResponseHandlers.json());
    Response cqlResponse = cqlCF.get(5, SECONDS);
    context.assertEquals(cqlResponse.code, HTTP_OK);
    log.info(cqlResponse.body
      + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
    context.assertTrue(isSizeMatch(cqlResponse, 1));

    /*
      delete a group - should fail as there is a user associated with the group
     */
    String delete1URL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> delete1CF = send(delete1URL, DELETE, null, HTTPResponseHandlers.empty());
    Response delete1Response = delete1CF.get(5, SECONDS);
    context.assertEquals(delete1Response.code, HTTP_BAD_REQUEST);
    log.info(delete1Response.body
      + "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

    /*
      delete a nonexistent group - should return 404
     */
    String deleteNEGURL = groupUrl + "/a492ffd2-b848-48bf-b716-1a645822279e";
    CompletableFuture<Response> deleteNEGCF = send(deleteNEGURL, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteNEGResponse = deleteNEGCF.get(5, SECONDS);
    context.assertEquals(deleteNEGResponse.code, HTTP_NOT_FOUND);
    log.info(deleteNEGResponse.body
      + "\nStatus - " + deleteNEGResponse.code + " at " + System.currentTimeMillis() + " for " + deleteNEGURL);

    /*
      try to add a duplicate group
     */
    CompletableFuture<Response> dupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response dupResponse = dupCF.get(5, SECONDS);
    context.assertEquals(dupResponse.code, 422);
    log.info(dupResponse.body
      + "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      get a group
     */
    String getSpecGroupURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> getSpecGroupCF = send(getSpecGroupURL, GET, null, HTTPResponseHandlers.json());
    Response getSpecGroupResponse = getSpecGroupCF.get(5, SECONDS);
    context.assertEquals(getSpecGroupResponse.code, HTTP_OK);
    log.info(getSpecGroupResponse.body
      + "\nStatus - " + getSpecGroupResponse.code + " at " + System.currentTimeMillis() + " for " + getSpecGroupURL);
    context.assertTrue("librarianFOO".equals(getSpecGroupResponse.body.getString("group")));

    /*
      get a group bad id
     */
    String getBadIDURL = groupUrl + "/3748ec8d-8dbc-4717-819d-87c839e6905e";
    CompletableFuture<Response> getBadIDCF = send(getBadIDURL, GET, null, HTTPResponseHandlers.empty());
    Response getBadIDResponse = getBadIDCF.get(5, SECONDS);
    context.assertEquals(getBadIDResponse.code, HTTP_NOT_FOUND);
    log.info(getBadIDResponse.body
      + "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

    /*
      delete a group with users should fail
     */
    String delete = groupUrl + "/" + groupID1;
    CompletableFuture<Response> deleteCF = send(delete, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteResponse = deleteCF.get(5, SECONDS);
    context.assertEquals(deleteResponse.code, HTTP_BAD_REQUEST);
    log.info(deleteResponse.body
      + "\nStatus - " + deleteResponse.code + " at " + System.currentTimeMillis() + " for " + delete);

    /* Create a user with a past-due expiration date */
    UUID expiredUserId = UUID.randomUUID();
    {
      Date now = new Date();
      Date pastDate = new Date(now.getTime() - (10 * 24 * 60 * 60 * 1000));
      String dateString = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").format(pastDate);
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
      CompletableFuture<Response> addExpiredUserCF = send(addUserURL, POST, expiredUserJson.encode(), HTTPResponseHandlers.json());
      Response addExpiredUserResponse = addExpiredUserCF.get(5, SECONDS);
      log.info(addExpiredUserResponse.body
        + "\nStatus - " + addExpiredUserResponse.code + " at "
        + System.currentTimeMillis() + " for " + addUserURL + " (addExpiredUser)");
      context.assertEquals(addExpiredUserResponse.code, 201);
      CompletableFuture<Void> getExpirationCF = new CompletableFuture();
      ExpirationTool.doExpirationForTenant(RestITSupport.vertx(), RestITSupport.context(), "diku").setHandler(
        res -> getExpirationCF.complete(null));
      getExpirationCF.get(5, SECONDS);
      //TimeUnit.SECONDS.sleep(15);
      CompletableFuture<Response> getExpiredUserCF = send(addUserURL + "/" + expiredUserId.toString(), GET, null,
        HTTPResponseHandlers.json());
      Response getExpiredUserResponse = getExpiredUserCF.get(5, SECONDS);
      context.assertEquals(getExpiredUserResponse.body.getBoolean("active"), false);
    }
  }

  private CompletableFuture<Response> send(String url, HttpMethod method, String content,
                                           Function<HttpResponse<Buffer>, Response> handler) {
    Future<HttpResponse<Buffer>> httpResponse;

    switch (method) {
      case GET:
        httpResponse = get(url);
        break;
      case POST:
        httpResponse = post(url, defaultString(content));
        break;
      case PUT:
        httpResponse = put(url, defaultString(content));
        break;
      case DELETE:
        httpResponse = delete(url);
        break;
      default:
        throw new IllegalArgumentException("Illegal method: " + method);
    }

    CompletableFuture<Response> result = new CompletableFuture<>();

    httpResponse.map(handler)
      .onSuccess(result::complete)
      .onFailure(result::completeExceptionally);

    return result;
  }

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

  private boolean isSizeMatch(Response r, int size) {
    return r.body.getInteger("totalRecords") == size;
  }

  private static class Response {

    int code;
    JsonObject body;
  }

  private void checkExistingLimitsByIds(TestContext context, String... expectedIds)
    throws InterruptedException, ExecutionException, TimeoutException {

    Response response = send(limitsUrl, GET, null, HTTPResponseHandlers.json())
      .get(5, SECONDS);
    context.assertEquals(response.code, HTTP_OK);

    Set<String> actualLimitIds = response.body
      .getJsonArray("patronBlockLimits")
      .stream()
      .map(JsonObject.class::cast)
      .map(limit -> limit.getString("id"))
      .collect(Collectors.toSet());

    Set<String> expectedLimitIds = new HashSet<>(Arrays.asList(expectedIds));

    context.assertEquals(expectedLimitIds, actualLimitIds);
  }

  private static class HTTPResponseHandlers {

    static Function<HttpResponse<Buffer>, Response> empty() {
      return response -> {
        Response result = new Response();
        result.code = response.statusCode();
        return result;
      };
    }

    static Function<HttpResponse<Buffer>, Response> json() {
      return response -> {
        Response result = new Response();
        result.code = response.statusCode();
        result.body = response.bodyAsJsonObject();
        return result;
      };
    }
  }

}
