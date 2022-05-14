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
import static org.folio.HttpStatus.HTTP_UNPROCESSABLE_ENTITY;
import static org.folio.moduserstest.RestITSupport.HTTP_LOCALHOST;
import static org.folio.moduserstest.RestITSupport.delete;
import static org.folio.moduserstest.RestITSupport.get;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.put;
import static org.folio.util.StringUtil.urlEncode;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.notNullValue;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.parser.JsonPathParser;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.utils.ExpirationTool;
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
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpResponse;
import lombok.SneakyThrows;

@RunWith(VertxUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class GroupIT {

  private final String userUrl = HTTP_LOCALHOST + RestITSupport.port() + "/users";
  private final String groupUrl = HTTP_LOCALHOST + RestITSupport.port() + "/groups";

  private static final String fooGroupData = "{\"group\": \"librarianFOO\",\"desc\": \"yet another basic lib group\", \"expirationOffsetInDays\": 365}";
  private static final String barGroupData = "{\"group\": \"librarianBAR\",\"desc\": \"and yet another basic lib group\"}";

  private static final Logger log = LogManager.getLogger(GroupIT.class);

  private static int userInc = 0;

  private static Vertx vertx;

  @Rule
  public Timeout rule = Timeout.seconds(20);

  @BeforeClass
  public static void setup(TestContext context) {
    vertx = Vertx.vertx();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    Integer port = NetworkUtils.nextFreePort();
    RestITSupport.setUp(port);
    TenantClient tenantClient = new TenantClient("http://localhost:" + Integer.toString(port), "diku", "diku");
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port));

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

  @Test
  @SneakyThrows
  public void test2Group() {
    /*
      add a group
     */
    CompletableFuture<Response> addGroupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response addGroupResponse = addGroupCF.get(5, SECONDS);
    assertThat(addGroupResponse.code, is(HTTP_CREATED));
    String groupID1 = addGroupResponse.body.getString("id");
    log.info(addGroupResponse.body
      + "\nStatus - " + addGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      update a group
     */
    String updateGroupURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> updateGroupCF = send(updateGroupURL, PUT, barGroupData, HTTPResponseHandlers.empty());
    Response updateGroupResponse = updateGroupCF.get(5, SECONDS);
    assertThat(updateGroupResponse.code, is(HTTP_NO_CONTENT));
    log.info(updateGroupResponse.body
      + "\nStatus - " + updateGroupResponse.code + " at " + System.currentTimeMillis() + " for " + updateGroupURL);

    /*
      delete a group
     */
    String deleteCleanURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> deleteCleanCF = send(deleteCleanURL, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteCleanResponse = deleteCleanCF.get(5, SECONDS);
    assertThat(deleteCleanResponse.code, is(HTTP_NO_CONTENT));
    log.info(deleteCleanResponse.body
      + "\nStatus - " + deleteCleanResponse.code + " at " + System.currentTimeMillis() + " for " + deleteCleanURL);

    /*
      re-add a group
     */
    CompletableFuture<Response> addNewGroupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response addNewGroupResponse = addNewGroupCF.get(5, SECONDS);
    assertThat(addNewGroupResponse.code, is(HTTP_CREATED));
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
    assertThat(addUserResponse.code, is(HTTP_CREATED));
    String userID = addUserResponse.body.getString("id");
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user name again
     */
    CompletableFuture<Response> addUserCF2 = send(addUserURL, POST, createUser(null, "jhandley", groupID1).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    assertThat(addUserResponse2.code, is(422));
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add the same user again with same id
     */
    CompletableFuture<Response> addUserCF3 = send(addUserURL, POST, createUser(userID, "jhandley", groupID1).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse3 = addUserCF3.get(5, SECONDS);
    assertThat(addUserResponse3.code, is(422));
    log.info(addUserResponse3.body
      + "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with non existent patron group
     */
    CompletableFuture<Response> addUserCF4 = send(addUserURL, POST,
      createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4 = addUserCF4.get(5, SECONDS);
    assertThat(addUserResponse4.code, is(HTTP_BAD_REQUEST));
    log.info(addUserResponse4.body
      + "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      add a user again with invalid uuid
     */
    CompletableFuture<Response> addUserCF4a = send(addUserURL, POST, createUser(null, "jhandley2nd", "invalid-uuid").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4a = addUserCF4a.get(5, SECONDS);
    assertThat(addUserResponse4a.code, is(HTTP_UNPROCESSABLE_ENTITY.toInt()));
    log.info(addUserResponse4a.body
      + "\nStatus - " + addUserResponse4a.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

    /*
      update a user again with non existent patron group
     */
    CompletableFuture<Response> updateUserCF = send(addUserURL + "/" + userID, PUT, createUser(userID, "jhandley2nd",
      "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(), HTTPResponseHandlers.empty());
    Response updateUserResponse = updateUserCF.get(5, SECONDS);
    assertThat(updateUserResponse.code, is(HTTP_BAD_REQUEST));
    log.info(updateUserResponse.body
      + "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      update a user again with existent patron group
     */
    CompletableFuture<Response> updateUser2CF = send(addUserURL + "/" + userID, PUT,
      createUser(userID, "jhandley2nd", groupID1).encode(),
      HTTPResponseHandlers.empty());
    Response updateUser2Response = updateUser2CF.get(5, SECONDS);
    assertThat(updateUser2Response.code, is(HTTP_NO_CONTENT));
    log.info(updateUser2Response.body
      + "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + addUserURL + "/" + userID);

    /*
      get all users belonging to a specific group
     */
    String getUsersInGroupURL = userUrl + "?query=patronGroup==" + groupID1;
    CompletableFuture<Response> getUsersInGroupCF = send(getUsersInGroupURL, GET, null,
      HTTPResponseHandlers.json());
    Response getUsersInGroupResponse = getUsersInGroupCF.get(5, SECONDS);
    assertThat(getUsersInGroupResponse.code, is(HTTP_OK));
    log.info(getUsersInGroupResponse.body
      + "\nStatus - " + getUsersInGroupResponse.code + " at " + System.currentTimeMillis() + " for "
      + getUsersInGroupURL);
    assertThat(getUsersInGroupResponse.body.getInteger("totalRecords"), is(1));

    /*
      get all groups in groups table
     */
    CompletableFuture<Response> getAllGroupCF = send(groupUrl, GET, null, HTTPResponseHandlers.json());
    Response getAllGroupResponse = getAllGroupCF.get(5, SECONDS);
    assertThat(getAllGroupResponse.code, is(HTTP_OK));
    log.info(getAllGroupResponse.body
      + "\nStatus - " + getAllGroupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);
    assertThat(getAllGroupResponse.body.getInteger("totalRecords"), is(5)); // 4 in reference + 1 in test

    /*
      try to get via cql
     */
    String cqlURL = groupUrl + "?query=group==librarianFOO";
    CompletableFuture<Response> cqlCF = send(cqlURL, GET, null, HTTPResponseHandlers.json());
    Response cqlResponse = cqlCF.get(5, SECONDS);
    assertThat(cqlResponse.code, is(HTTP_OK));
    log.info(cqlResponse.body
      + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
    assertThat(cqlResponse.body.getInteger("totalRecords"), is(1));

    /*
      delete a group - should fail as there is a user associated with the group
     */
    String delete1URL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> delete1CF = send(delete1URL, DELETE, null, HTTPResponseHandlers.empty());
    Response delete1Response = delete1CF.get(5, SECONDS);
    assertThat(delete1Response.code, is(HTTP_BAD_REQUEST));
    log.info(delete1Response.body
      + "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

    /*
      delete a nonexistent group - should return 404
     */
    String deleteNEGURL = groupUrl + "/a492ffd2-b848-48bf-b716-1a645822279e";
    CompletableFuture<Response> deleteNEGCF = send(deleteNEGURL, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteNEGResponse = deleteNEGCF.get(5, SECONDS);
    assertThat(deleteNEGResponse.code, is(HTTP_NOT_FOUND));
    log.info(deleteNEGResponse.body
      + "\nStatus - " + deleteNEGResponse.code + " at " + System.currentTimeMillis() + " for " + deleteNEGURL);

    /*
      try to add a duplicate group
     */
    CompletableFuture<Response> dupCF = send(groupUrl, POST, fooGroupData, HTTPResponseHandlers.json());
    Response dupResponse = dupCF.get(5, SECONDS);
    assertThat(dupResponse.code, is(422));
    log.info(dupResponse.body
      + "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      get a group
     */
    String getSpecGroupURL = groupUrl + "/" + groupID1;
    CompletableFuture<Response> getSpecGroupCF = send(getSpecGroupURL, GET, null, HTTPResponseHandlers.json());
    Response getSpecGroupResponse = getSpecGroupCF.get(5, SECONDS);
    assertThat(getSpecGroupResponse.code, is(HTTP_OK));
    log.info(getSpecGroupResponse.body
      + "\nStatus - " + getSpecGroupResponse.code + " at " + System.currentTimeMillis() + " for " + getSpecGroupURL);
    assertThat(getSpecGroupResponse.body.getString("group"), is("librarianFOO"));

    /*
      get a group bad id
     */
    String getBadIDURL = groupUrl + "/3748ec8d-8dbc-4717-819d-87c839e6905e";
    CompletableFuture<Response> getBadIDCF = send(getBadIDURL, GET, null, HTTPResponseHandlers.empty());
    Response getBadIDResponse = getBadIDCF.get(5, SECONDS);
    assertThat(getBadIDResponse.code, is(HTTP_NOT_FOUND));
    log.info(getBadIDResponse.body
      + "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

    /*
      delete a group with users should fail
     */
    String delete = groupUrl + "/" + groupID1;
    CompletableFuture<Response> deleteCF = send(delete, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteResponse = deleteCF.get(5, SECONDS);
    assertThat(deleteResponse.code, is(HTTP_BAD_REQUEST));
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
      assertThat(addExpiredUserResponse.code, is(201));
      CompletableFuture<Void> getExpirationCF = new CompletableFuture<Void>();
      ExpirationTool.doExpirationForTenant(RestITSupport.vertx(), "diku")
        .onComplete(res -> getExpirationCF.complete(null));
      getExpirationCF.get(5, SECONDS);
      //TimeUnit.SECONDS.sleep(15);
      CompletableFuture<Response> getExpiredUserCF = send(addUserURL + "/" + expiredUserId.toString(), GET, null,
        HTTPResponseHandlers.json());
      Response getExpiredUserResponse = getExpiredUserCF.get(5, SECONDS);
      assertThat(getExpiredUserResponse.body.getBoolean("active"), is(false));
    }

    CompletableFuture<Response> postGroupCF = send(groupUrl, POST, barGroupData, HTTPResponseHandlers.json());
    Response postGroupResponse = postGroupCF.get(5, SECONDS);
    assertThat(postGroupResponse.code, is(HTTP_CREATED));
    String barGroupId = postGroupResponse.body.getString("id");
    assertThat(barGroupId, is(notNullValue()));

    int inc = 0;
    addUserCF = send(userUrl, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      HTTPResponseHandlers.json());
    addUserResponse = addUserCF.get(5, SECONDS);
    assertThat(addUserResponse.code, is(HTTP_CREATED));
    log.info(addUserResponse.body
      + "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    addUserCF2 = send(userUrl, POST, createUser(null, "jhandley" + inc++, barGroupId).encode(),
      HTTPResponseHandlers.json());
    addUserResponse2 = addUserCF2.get(5, SECONDS);
    assertThat(addUserResponse2.code, is(HTTP_CREATED));
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
      cqlURL = urls[i];
      CompletableFuture<Response> cf = send(cqlURL, GET, null, HTTPResponseHandlers.json());

      cqlResponse = cf.get(5, SECONDS);
      assertThat(cqlResponse.code, is(HTTP_OK));
      log.info(cqlResponse.body
        + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL + " (url" + (i) + ") : " + cqlResponse.body.toString());
      //requests should usually have 3 or 4 results
      switch (i) {
        case 5:
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(1));
          break;
        case 7:
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(0));
          break;
        case 6:
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(greaterThan(2)));
          break;
        case 1:
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(4));
          assertThat(cqlResponse.body.getJsonArray("users").getJsonObject(0).getString("username"), is("jhandley2nd"));
          break;
        case 2:
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(4));
          assertThat(cqlResponse.body.getJsonArray("users").getJsonObject(0).getString("username"), is("jhandley0"));
          break;
        case 4:
          assertThat(((String) (new JsonPathParser(cqlResponse.body).getValueAt("users[0].personal.lastName"))), startsWith("Triangle"));
          break;
        case 0:
          //Baseline test
          int totalRecords = cqlResponse.body.getInteger("totalRecords");
          assertThat(totalRecords, is(4));
          break;
        default:
          // This mimics the previous range assertion, should be replaced with more specific assertions
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(greaterThanOrEqualTo(0)));
          assertThat(cqlResponse.body.getInteger("totalRecords"), is(lessThanOrEqualTo(4)));
          break;
      }
    }
  }

  private CompletableFuture<Response> send(String url, HttpMethod method, String content,
      Function<HttpResponse<Buffer>, Response> handler) {

    Future<HttpResponse<Buffer>> httpResponse;

    switch (method.name()) {
      case "GET":
        httpResponse = get(url);
        break;
      case "POST":
        httpResponse = post(url, defaultString(content));
        break;
      case "PUT":
        httpResponse = put(url, defaultString(content));
        break;
      case "DELETE":
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

  private static class Response {
    int code;
    JsonObject body;
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
