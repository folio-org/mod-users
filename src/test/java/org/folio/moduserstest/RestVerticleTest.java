package org.folio.moduserstest;

import java.net.HttpURLConnection;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientRequest;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";

  private static String postRequest = "{\"group\": \"librarianPOST\",\"desc\": \"basic lib group\"}";
  private static String putRequest = "{\"group\": \"librarianPUT\",\"desc\": \"basic lib group\"}";
  private static String createUserRequest =
      "{ \"username\": \"jhandey\" , \"id\": \"7261ecaae3a74dc68b468e12a70b1aec\"}";

  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(180);  // 3 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("localhost", port, "diku");
    vertx = Vertx.vertx();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port));
    try {
      PostgresClient.setIsEmbedded(true);
      PostgresClient.getInstance(vertx).startEmbeddedPostgres();
    } catch(Exception e) {
      e.printStackTrace();
      context.fail(e);
      return;
    }
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      try {
        tenantClient.post(null, res2 -> {
           async.complete();
        });
      } catch(Exception e) {
        e.printStackTrace();
      }

    });
  }

  @AfterClass
  public static void teardown(TestContext context) {
    context.async().complete();
  }

  private Future<Void> getEmptyUsers(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(buf -> {
          String body = buf.toString();
          future.fail("Bad status code: " + res.statusCode() + " : " + body);
        });
      } else {
        res.bodyHandler(buf -> {
          JsonObject userCollectionObject = buf.toJsonObject();
          if(userCollectionObject.getJsonArray("users").size() == 0 &&
                  userCollectionObject.getInteger("total_records") == 00) {
            future.complete();
          } else {
            future.fail("Invalid return JSON: " + buf.toString());
          }
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end();
    return future;
  }

  private Future<Void> postUser(TestContext context) {
    Future future = Future.future();
    JsonObject userObject = new JsonObject()
            .put("username", "joeblock")
            .put("id", "1234567")
            .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if(res.statusCode() >= 200 && res.statusCode() < 300) {
        future.complete();
      } else {
        future.fail("Got status code: " + res.statusCode());
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end(userObject.encode());
    return future;
  }

 private Future<Void> getUser(TestContext context) {
   Future future = Future.future();
   HttpClient client = vertx.createHttpClient();
   client.get(port, "localhost", "/users/1234567", res -> {
     if(res.statusCode() == 200) {
       res.bodyHandler(buf -> {
         JsonObject userObject = buf.toJsonObject();
         if(userObject.getString("username").equals("joeblock")) {
           future.complete();
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
           .end();
   return future;
 }

 @Test
  public void doSequentialTests(TestContext context) {
    Async async = context.async();
    Future<Void> startFuture;
    Future<Void> f1 = Future.future();
    getEmptyUsers(context).setHandler(f1.completer());
    startFuture = f1.compose(v -> {
      Future<Void> f2 = Future.future();
      postUser(context).setHandler(f2.completer());
      return f2;
    }).compose(v -> {
      Future<Void> f3 = Future.future();
      getUser(context).setHandler(f3.completer());
      return f3;
    });

    startFuture.setHandler(res -> {
      if(res.succeeded()) {
        async.complete();
      } else {
        context.fail(res.cause());
      }
    });
  }

 @Test
 public void testGroup(TestContext context){
   String url = "http://localhost:"+port+"/groups";
   try {
    /**add a group*/
     CompletableFuture<Response> addGroupCF = new CompletableFuture();
     String addGroupURL = url;
     send(addGroupURL, context, HttpMethod.POST, postRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addGroupCF));
     Response addGroupResponse = addGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addGroupResponse.code, HttpURLConnection.HTTP_CREATED);
     String groupID = addGroupResponse.body.getString("_id");
     System.out.println(addGroupResponse.body +
       "\nStatus - " + addGroupResponse.code + " at " + System.currentTimeMillis() + " for " + addGroupURL);

     /**update a group*/
     CompletableFuture<Response> updateGroupCF = new CompletableFuture();
     String updateGroupURL = url +"/"+groupID;
     send(updateGroupURL, context, HttpMethod.PUT, putRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 204,  new HTTPNoBodyResponseHandler(updateGroupCF));
     Response updateGroupResponse = updateGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(updateGroupResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
     System.out.println(updateGroupResponse.body +
       "\nStatus - " + updateGroupResponse.code + " at " + System.currentTimeMillis() + " for " + updateGroupURL);

    /**add a user*/
     CompletableFuture<Response> addUserCF = new CompletableFuture();
     String addUserURL = "http://localhost:"+port+"/users";
     send(addUserURL, context, HttpMethod.POST, createUserRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addUserCF));
     Response addUserResponse = addUserCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse.code, HttpURLConnection.HTTP_CREATED);
     String userID = addUserResponse.body.getString("id");
     System.out.println(addUserResponse.body +
       "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**add the same user again*/
     CompletableFuture<Response> addUserCF2 = new CompletableFuture();
     send(addUserURL, context, HttpMethod.POST, createUserRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addUserCF2));
     Response addUserResponse2 = addUserCF2.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse2.code, 422);
     System.out.println(addUserResponse2.body +
       "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**add a user to the group*/
     CompletableFuture<Response> addUser2GroupCF = new CompletableFuture();
     String addUser2GroupURL = url+"/"+groupID+"/users/7261ecaae3a74dc68b468e12a70b1aec";
     send(addUser2GroupURL, context,
       HttpMethod.PUT, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(addUser2GroupCF));
     Response addUser2GroupResponse = addUser2GroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUser2GroupResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
     System.out.println(addUser2GroupResponse.body +
       "\nStatus - " + addUser2GroupResponse.code + " at " + System.currentTimeMillis() + " for "
         + addUser2GroupURL);

     /**get all users belonging to a specific group*/
     CompletableFuture<Response> getUsersInGroupCF = new CompletableFuture();
     String getUsersInGroupURL = url+"/"+groupID+"/users";
     send(getUsersInGroupURL, context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF,
       200, new HTTPResponseHandler(getUsersInGroupCF));
     Response getUsersInGroupResponse = getUsersInGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(getUsersInGroupResponse.code, HttpURLConnection.HTTP_OK);
     System.out.println(getUsersInGroupResponse.body +
       "\nStatus - " + getUsersInGroupResponse.code + " at " + System.currentTimeMillis() + " for "
         + getUsersInGroupURL);
     context.assertTrue(isSizeMatch(getUsersInGroupResponse, 1));

     /**get all groups in groups table*/
     CompletableFuture<Response> getAllGroupCF = new CompletableFuture();
     String getAllGroupURL = url;
     send(getAllGroupURL, context, HttpMethod.GET, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, new HTTPResponseHandler(getAllGroupCF));
     Response getAllGroupResponse = getAllGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(getAllGroupResponse.code, HttpURLConnection.HTTP_OK);
     System.out.println(getAllGroupResponse.body +
       "\nStatus - " + getAllGroupResponse.code + " at " + System.currentTimeMillis() + " for "
         + getAllGroupURL);
     context.assertTrue(isSizeMatch(getAllGroupResponse, 1));

     /**get groups belonging to a user*/
     CompletableFuture<Response> getAllGroup4UserCF = new CompletableFuture();
     String getAllGroup4UserURL = "http://localhost:"+port+"/users/7261ecaae3a74dc68b468e12a70b1aec/groups";
     send(getAllGroup4UserURL,
       context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
       new HTTPResponseHandler(getAllGroup4UserCF));
     Response getAllGroup4UserResponse = getAllGroup4UserCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(getAllGroup4UserResponse.code, HttpURLConnection.HTTP_OK);
     System.out.println(getAllGroup4UserResponse.body +
       "\nStatus - " + getAllGroup4UserResponse.code + " at " + System.currentTimeMillis() + " for "
         + getAllGroup4UserURL);
     context.assertTrue(isSizeMatch(getAllGroup4UserResponse, 1));

     /**try to get via cql*/
     CompletableFuture<Response> cqlCF = new CompletableFuture();
     String cqlURL = url+"/"+groupID+"/users?query=username==jhandey";
     send(cqlURL,
       context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
       new HTTPResponseHandler(cqlCF));
     Response cqlResponse = cqlCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(cqlResponse.code, HttpURLConnection.HTTP_OK);
     System.out.println(cqlResponse.body +
       "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
     context.assertTrue(isSizeMatch(cqlResponse, 1));

     /**delete a group - should fail as there is a user associated with the group*/
     CompletableFuture<Response> delete1CF = new CompletableFuture();
     String delete1URL = url+"/"+groupID;
     send(delete1URL, context, HttpMethod.DELETE, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(delete1CF));
     Response delete1Response = delete1CF.get(5, TimeUnit.SECONDS);
     context.assertEquals(delete1Response.code, HttpURLConnection.HTTP_BAD_REQUEST);
     System.out.println(delete1Response.body +
       "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

     /**request users from a non existant group*/
     CompletableFuture<Response> badRequestCF = new CompletableFuture();
     String badRequestURL = url+"/"+groupID+"abc/users";
     send(badRequestURL, context, HttpMethod.GET, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, new HTTPResponseHandler(badRequestCF));
     Response bad1Response = badRequestCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(bad1Response.code, HttpURLConnection.HTTP_OK);
     System.out.println(bad1Response.body +
       "\nStatus - " + bad1Response.code + " at " + System.currentTimeMillis() + " for " + badRequestURL);
     context.assertEquals(0, bad1Response.body.getJsonArray("users").size());

     /**delete all users in a group*/
     CompletableFuture<Response> delAllUCF = new CompletableFuture();
     String delAllURL = url+"/"+groupID+"/users";
     send(delAllURL, context, HttpMethod.DELETE, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(delAllUCF));
     Response delAllUResponse = delAllUCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(delAllUResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
     System.out.println(delAllUResponse.body +
       "\nStatus - " + delAllUResponse.code + " at " + System.currentTimeMillis() + " for " + delAllURL);

     /**try to add a duplicate group*/
     CompletableFuture<Response> dupCF = new CompletableFuture();
     send(url, context, HttpMethod.POST, putRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPResponseHandler(dupCF));
     Response dupResponse = dupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(dupResponse.code, 422);
     System.out.println(dupResponse.body +
       "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + url);

     /**get a group*/
     CompletableFuture<Response> getSpecGroupCF = new CompletableFuture();
     String getSpecGroupURL = url+"/"+groupID;
     send(getSpecGroupURL, context, HttpMethod.GET, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, new HTTPResponseHandler(getSpecGroupCF));
     Response getSpecGroupResponse = getSpecGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(getSpecGroupResponse.code, HttpURLConnection.HTTP_OK);
     System.out.println(getSpecGroupResponse.body +
       "\nStatus - " + getSpecGroupResponse.code + " at " + System.currentTimeMillis() + " for " + getSpecGroupURL);
     context.assertTrue("librarianPUT".equals(getSpecGroupResponse.body.getString("group")));

     /**get a group bad id*/
     CompletableFuture<Response> getBadIDCF = new CompletableFuture();
     String getBadIDURL = url+"/12345678";
     send(getBadIDURL, context, HttpMethod.GET, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 404, new HTTPNoBodyResponseHandler(getBadIDCF));
     Response getBadIDResponse = getBadIDCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(getBadIDResponse.code, HttpURLConnection.HTTP_NOT_FOUND);
     System.out.println(getBadIDResponse.body +
       "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

     /**put user for next dup test*/
     CompletableFuture<Response> d1CF = new CompletableFuture();
     String d1 = url+"/"+groupID+"/users/7261ecaae3a74dc68b468e12a70b1aec";
     send(d1, context,
       HttpMethod.PUT, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(d1CF));
     Response d1Response = d1CF.get(5, TimeUnit.SECONDS);
     context.assertEquals(d1Response.code, HttpURLConnection.HTTP_NO_CONTENT);
     System.out.println(d1Response.body +
       "\nStatus - " + d1Response.code + " at " + System.currentTimeMillis() + " for " + d1);

     /**duplicate a user to a group*/
     CompletableFuture<Response> d2CF = new CompletableFuture();
     send(d1, context, HttpMethod.PUT, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,
       new HTTPResponseHandler(d2CF));
     Response d2Response = d2CF.get(5, TimeUnit.SECONDS);
     context.assertEquals(d2Response.code, 422);
     System.out.println(d2Response.body +
       "\nStatus - " + d2Response.code + " at " + System.currentTimeMillis() + " for " + d1);

     /**delete a group with users should fail*/
     CompletableFuture<Response> deleteCF = new CompletableFuture();
     String delete = url+"/"+groupID;
     send(delete, context, HttpMethod.DELETE, null,
     SUPPORTED_CONTENT_TYPE_JSON_DEF, 400, new HTTPNoBodyResponseHandler(deleteCF));
     Response deleteResponse = deleteCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(deleteResponse.code, HttpURLConnection.HTTP_BAD_REQUEST);
     System.out.println(deleteResponse.body +
       "\nStatus - " + deleteResponse.code + " at " + System.currentTimeMillis() + " for " + delete);


  } catch (Exception e) {
    e.printStackTrace();
    context.fail(e.getMessage());
  }
 }

 private void send(String url, TestContext context, HttpMethod method, String content,
     String contentType, int errorCode, Handler<HttpClientResponse> handler) {
   HttpClient client = vertx.createHttpClient();
   HttpClientRequest request;
   if(content == null){
     content = "";
   }
   Buffer buffer = Buffer.buffer(content);

   if (method == HttpMethod.POST) {
     request = client.postAbs(url);
   }
   else if (method == HttpMethod.DELETE) {
     request = client.deleteAbs(url);
   }
   else if (method == HttpMethod.GET) {
     request = client.getAbs(url);
   }
   else {
     request = client.putAbs(url);
   }
   request.exceptionHandler(error -> {
     context.fail(error.getMessage());
   })
   .handler(handler);
   request.putHeader("Authorization", "diku");
   request.putHeader("x-okapi-tenant", "diku");
   request.putHeader("Accept", "application/json,text/plain");
   request.putHeader("Content-type", contentType);
   request.end(buffer);
 }

 class HTTPResponseHandler implements Handler<HttpClientResponse> {

   CompletableFuture<Response> event;
   public HTTPResponseHandler(CompletableFuture<Response> cf){
     event = cf;
   }
   @Override
   public void handle(HttpClientResponse hcr) {
     hcr.bodyHandler( bh -> {
       Response r = new Response();
       r.code = hcr.statusCode();
       r.body = bh.toJsonObject();
       event.complete(r);
     });
   }
 }

 class HTTPNoBodyResponseHandler implements Handler<HttpClientResponse> {

   CompletableFuture<Response> event;
   public HTTPNoBodyResponseHandler(CompletableFuture<Response> cf){
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

 private boolean isSizeMatch(Response r, int size){
   if(r.body.getInteger("total_records") == size){
     return true;
   }
   return false;
 }

}

