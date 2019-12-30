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
import static org.folio.moduserstest.RestITSupport.HTTP_LOCALHOST;
import static org.folio.moduserstest.RestITSupport.fail;
import static org.folio.util.StringUtil.urlEncode;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.WebClient;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.parser.JsonPathParser;
import org.folio.rest.utils.ExpirationTool;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;


@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class GroupIT {

  private final String userUrl = HTTP_LOCALHOST + RestITSupport.port() + "/users";
  private final String groupUrl = HTTP_LOCALHOST + RestITSupport.port() + "/groups";

  private static final String fooGroupData = "{\"group\": \"librarianFOO\",\"desc\": \"yet another basic lib group\"}";
  private static final String barGroupData = "{\"group\": \"librarianBAR\",\"desc\": \"and yet another basic lib group\"}";

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
  public void test3CrossTableQueries(TestContext context) throws Exception {
    String url = HTTP_LOCALHOST + RestITSupport.port() + "/users?query=";

    CompletableFuture<Response> postGroupCF = new CompletableFuture();
    send(groupUrl, context, POST, barGroupData, new HTTPResponseHandler(postGroupCF));
    Response postGroupResponse = postGroupCF.get(5, SECONDS);
    context.assertEquals(postGroupResponse.code, HTTP_CREATED);
    String barGroupId = postGroupResponse.body.getString("id");
    context.assertNotNull(barGroupId);

    int inc = 0;
    CompletableFuture<Response> addUserCF = new CompletableFuture();
    send(userUrl, context, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      new HTTPResponseHandler(addUserCF));
    Response addUserResponse = addUserCF.get(5, SECONDS);
    context.assertEquals(addUserResponse.code, HTTP_CREATED);
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    CompletableFuture<Response> addUserCF2 = new CompletableFuture();
    send(userUrl, context, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      new HTTPResponseHandler(addUserCF2));
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    context.assertEquals(addUserResponse2.code, HTTP_CREATED);
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + userUrl);

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
    //query by tag, should get one record
    String url8 = url + urlEncode("tags=foo");

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
      send(cqlURL, context, GET, null, new HTTPResponseHandler(cf));
      Response cqlResponse = cf.get(5, SECONDS);
      context.assertEquals(cqlResponse.code, HTTP_OK);
      log.info(cqlResponse.body
        + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL + " (url" + (i) + ") : " + cqlResponse.body.toString());
      //requests should usually have 3 or 4 results
      switch (i) {
        case 8:
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
    CompletableFuture<Response> addGroupCF = new CompletableFuture();
    send(groupUrl, context, POST, fooGroupData, new HTTPResponseHandler(addGroupCF));
    Response addGroupResponse = addGroupCF.get(5, SECONDS);
    context.assertEquals(addGroupResponse.code, HTTP_CREATED);
    String groupID1 = addGroupResponse.body.getString("id");
    log.info(addGroupResponse.body
      + "\nStatus - " + addGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      update a group
     */
    CompletableFuture<Response> updateGroupCF = new CompletableFuture();
    String updateGroupURL = groupUrl + "/" + groupID1;
    send(updateGroupURL, context, PUT, barGroupData, new HTTPNoBodyResponseHandler(updateGroupCF));
    Response updateGroupResponse = updateGroupCF.get(5, SECONDS);
    context.assertEquals(updateGroupResponse.code, HTTP_NO_CONTENT);
    log.info(updateGroupResponse.body
      + "\nStatus - " + updateGroupResponse.code + " at " + System.currentTimeMillis() + " for " + updateGroupURL);

    /*
      delete a group
     */
    CompletableFuture<Response> deleteCleanCF = new CompletableFuture();
    String deleteCleanURL = groupUrl + "/" + groupID1;
    send(deleteCleanURL, context, DELETE, null, new HTTPNoBodyResponseHandler(deleteCleanCF));
    Response deleteCleanResponse = deleteCleanCF.get(5, SECONDS);
    context.assertEquals(deleteCleanResponse.code, HTTP_NO_CONTENT);
    log.info(deleteCleanResponse.body
      + "\nStatus - " + deleteCleanResponse.code + " at " + System.currentTimeMillis() + " for " + deleteCleanURL);

    /*
      re-add a group
     */
    CompletableFuture<Response> addNewGroupCF = new CompletableFuture();
    send(groupUrl, context, POST, fooGroupData, new HTTPResponseHandler(addNewGroupCF));
    Response addNewGroupResponse = addNewGroupCF.get(5, SECONDS);
    context.assertEquals(addNewGroupResponse.code, HTTP_CREATED);
    groupID1 = addNewGroupResponse.body.getString("id");
    log.info(addNewGroupResponse.body
      + "\nStatus - " + addNewGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      add a user
     */
    CompletableFuture<Response> addUserCF = new CompletableFuture();
    String addUserURL = userUrl;
    send(addUserURL, context, POST, createUser(null, "jhandley", groupID1).encode(),
      new HTTPResponseHandler(addUserCF));
    Response addUserResponse = addUserCF.get(5, SECONDS);
    context.assertEquals(addUserResponse.code, HTTP_CREATED);
    String userID = addUserResponse.body.getString("id");
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user name again
     */
    CompletableFuture<Response> addUserCF2 = new CompletableFuture();
    send(addUserURL, context, POST, createUser(null, "jhandley", groupID1).encode(),
      new HTTPResponseHandler(addUserCF2));
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    context.assertEquals(addUserResponse2.code, 422);
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user again with same id
     */
    CompletableFuture<Response> addUserCF3 = new CompletableFuture();
    send(addUserURL, context, POST, createUser(userID, "jhandley", groupID1).encode(),
      new HTTPResponseHandler(addUserCF3));
    Response addUserResponse3 = addUserCF3.get(5, SECONDS);
    context.assertEquals(addUserResponse3.code, 422);
    log.info(addUserResponse3.body
      + "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with non existent patron group
     */
    CompletableFuture<Response> addUserCF4 = new CompletableFuture();
    send(addUserURL, context, POST, createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      new HTTPNoBodyResponseHandler(addUserCF4));
    Response addUserResponse4 = addUserCF4.get(5, SECONDS);
    context.assertEquals(addUserResponse4.code, HTTP_BAD_REQUEST);
    log.info(addUserResponse4.body
      + "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with invalid uuid
     */
    CompletableFuture<Response> addUserCF4a = new CompletableFuture();
    send(addUserURL, context, POST, createUser(null, "jhandley2nd", "invalid-uuid").encode(),
      new HTTPNoBodyResponseHandler(addUserCF4a));
    Response addUserResponse4a = addUserCF4a.get(5, SECONDS);
    context.assertEquals(addUserResponse4a.code, HTTP_BAD_REQUEST);
    log.info(addUserResponse4a.body
      + "\nStatus - " + addUserResponse4a.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      update a user again with non existent patron group
     */
    CompletableFuture<Response> updateUserCF = new CompletableFuture();
    send(addUserURL + "/" + userID, context, PUT, createUser(userID, "jhandley2nd",
      "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(), new HTTPNoBodyResponseHandler(updateUserCF));
    Response updateUserResponse = updateUserCF.get(5, SECONDS);
    context.assertEquals(updateUserResponse.code, HTTP_BAD_REQUEST);
    log.info(updateUserResponse.body
      + "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      update a user again with existent patron group
     */
    CompletableFuture<Response> updateUser2CF = new CompletableFuture();
    send(addUserURL + "/" + userID, context, PUT, createUser(userID, "jhandley2nd", groupID1).encode(),
      new HTTPNoBodyResponseHandler(updateUser2CF));
    Response updateUser2Response = updateUser2CF.get(5, SECONDS);
    context.assertEquals(updateUser2Response.code, HTTP_NO_CONTENT);
    log.info(updateUser2Response.body
      + "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      get all users belonging to a specific group
     */
    CompletableFuture<Response> getUsersInGroupCF = new CompletableFuture();
    String getUsersInGroupURL = userUrl + "?query=patronGroup==" + groupID1;
    send(getUsersInGroupURL, context, GET, null,
      new HTTPResponseHandler(getUsersInGroupCF));
    Response getUsersInGroupResponse = getUsersInGroupCF.get(5, SECONDS);
    context.assertEquals(getUsersInGroupResponse.code, HTTP_OK);
    log.info(getUsersInGroupResponse.body
      + "\nStatus - " + getUsersInGroupResponse.code + " at " + System.currentTimeMillis() + " for "
      + getUsersInGroupURL);
    context.assertTrue(isSizeMatch(getUsersInGroupResponse, 1));

    /*
      get all groups in groups table
     */
    CompletableFuture<Response> getAllGroupCF = new CompletableFuture();
    send(groupUrl, context, GET, null, new HTTPResponseHandler(getAllGroupCF));
    Response getAllGroupResponse = getAllGroupCF.get(5, SECONDS);
    context.assertEquals(getAllGroupResponse.code, HTTP_OK);
    log.info(getAllGroupResponse.body
      + "\nStatus - " + getAllGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);
    context.assertTrue(isSizeMatch(getAllGroupResponse, 5)); // 4 in reference + 1 in test

    /*
      try to get via cql
     */
    CompletableFuture<Response> cqlCF = new CompletableFuture();
    String cqlURL = groupUrl + "?query=group==librarianFOO";
    send(cqlURL, context, GET, null, new HTTPResponseHandler(cqlCF));
    Response cqlResponse = cqlCF.get(5, SECONDS);
    context.assertEquals(cqlResponse.code, HTTP_OK);
    log.info(cqlResponse.body
      + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
    context.assertTrue(isSizeMatch(cqlResponse, 1));

    /*
      delete a group - should fail as there is a user associated with the group
     */
    CompletableFuture<Response> delete1CF = new CompletableFuture();
    String delete1URL = groupUrl + "/" + groupID1;
    send(delete1URL, context, DELETE, null, new HTTPNoBodyResponseHandler(delete1CF));
    Response delete1Response = delete1CF.get(5, SECONDS);
    context.assertEquals(delete1Response.code, HTTP_BAD_REQUEST);
    log.info(delete1Response.body
      + "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

    /*
      delete a nonexistent group - should return 404
     */
    CompletableFuture<Response> deleteNEGCF = new CompletableFuture();
    String deleteNEGURL = groupUrl + "/a492ffd2-b848-48bf-b716-1a645822279e";
    send(deleteNEGURL, context, DELETE, null, new HTTPNoBodyResponseHandler(deleteNEGCF));
    Response deleteNEGResponse = deleteNEGCF.get(5, SECONDS);
    context.assertEquals(deleteNEGResponse.code, HTTP_NOT_FOUND);
    log.info(deleteNEGResponse.body
      + "\nStatus - " + deleteNEGResponse.code + " at " + System.currentTimeMillis() + " for " + deleteNEGURL);

    /*
      try to add a duplicate group
     */
    CompletableFuture<Response> dupCF = new CompletableFuture();
    send(groupUrl, context, POST, fooGroupData, new HTTPResponseHandler(dupCF));
    Response dupResponse = dupCF.get(5, SECONDS);
    context.assertEquals(dupResponse.code, 422);
    log.info(dupResponse.body
      + "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      get a group
     */
    CompletableFuture<Response> getSpecGroupCF = new CompletableFuture();
    String getSpecGroupURL = groupUrl + "/" + groupID1;
    send(getSpecGroupURL, context, GET, null, new HTTPResponseHandler(getSpecGroupCF));
    Response getSpecGroupResponse = getSpecGroupCF.get(5, SECONDS);
    context.assertEquals(getSpecGroupResponse.code, HTTP_OK);
    log.info(getSpecGroupResponse.body
      + "\nStatus - " + getSpecGroupResponse.code + " at " + System.currentTimeMillis() + " for " + getSpecGroupURL);
    context.assertTrue("librarianFOO".equals(getSpecGroupResponse.body.getString("group")));

    /*
      get a group bad id
     */
    CompletableFuture<Response> getBadIDCF = new CompletableFuture();
    String getBadIDURL = groupUrl + "/3748ec8d-8dbc-4717-819d-87c839e6905e";
    send(getBadIDURL, context, GET, null, new HTTPNoBodyResponseHandler(getBadIDCF));
    Response getBadIDResponse = getBadIDCF.get(5, SECONDS);
    context.assertEquals(getBadIDResponse.code, HTTP_NOT_FOUND);
    log.info(getBadIDResponse.body
      + "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

    /*
      delete a group with users should fail
     */
    CompletableFuture<Response> deleteCF = new CompletableFuture();
    String delete = groupUrl + "/" + groupID1;
    send(delete, context, DELETE, null, new HTTPNoBodyResponseHandler(deleteCF));
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
      CompletableFuture<Response> addExpiredUserCF = new CompletableFuture();
      send(addUserURL, context, POST, expiredUserJson.encode(), new HTTPResponseHandler(addExpiredUserCF));
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
      CompletableFuture<Response> getExpiredUserCF = new CompletableFuture();
      send(addUserURL + "/" + expiredUserId.toString(), context, GET, null,
        new HTTPResponseHandler(getExpiredUserCF));
      Response getExpiredUserResponse = getExpiredUserCF.get(5, SECONDS);
      context.assertEquals(getExpiredUserResponse.body.getBoolean("active"), false);
    }
  }

  private void send(String url, TestContext context, HttpMethod method, String content, Handler<HttpClientResponse> handler) {
    /*HttpClient client = RestITSupport.vertx().createHttpClient();
    HttpClientRequest request;
    if (content == null) {
      content = "";
    }
    Buffer buffer = Buffer.buffer(content);

    if (method == POST) {
      request = client.postAbs(url);
    } else if (method == DELETE) {
      request = client.deleteAbs(url);
    } else if (method == GET) {
      request = client.getAbs(url);
    } else {
      request = client.putAbs(url);
    }
    request.exceptionHandler(error -> context.fail(error.getMessage()))
      .handler(handler);
    //request.putHeader("Authorization", "diku");
    request.putHeader("x-okapi-tenant", "diku");
    request.putHeader("Accept", SUPPORTED_CONTENT_TYPE_JSON_DEF);
    request.putHeader("Accept", SUPPORTED_CONTENT_TYPE_TEXT_DEF);
    request.putHeader("Content-type", SUPPORTED_CONTENT_TYPE_JSON_DEF);
    request.end(buffer);*/

    WebClient client = RestITSupport.webClient();
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

  private static class HTTPResponseHandler implements Handler<HttpClientResponse> {

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

  private static class HTTPNoBodyResponseHandler implements Handler<HttpClientResponse> {

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

  private static class Response {

    int code;
    JsonObject body;
  }

}
