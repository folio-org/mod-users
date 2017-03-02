package org.folio.moduserstest;

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

@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";

  private static String postRequest = "{\"group\": \"librarianPOST\",\"desc\": \"basic lib group\"}";
  private static String putRequest = "{\"group\": \"librarianPUT\",\"desc\": \"basic lib group\"}";
  private static String createUserRequest = "{ \"username\": \"jhandey\" , \"id\": \"7261ecaae3a74dc68b468e12a70b1aec\"}";

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
   Async async = context.async();
   String url = "http://localhost:"+port+"/groups";
   //add a group
   send(url, context, HttpMethod.POST, postRequest,
     SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, response -> {
       int statusCode = response.statusCode();
       System.out.println("Status - " + statusCode + " at " + System.currentTimeMillis() + " for " + url);
       context.assertEquals(201, statusCode);
       final String location = response.getHeader("Location");
       System.out.println("Location - " + location);
       //update a group
       send("http://localhost:"+port+location, context, HttpMethod.PUT, putRequest,
         SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, putResponse -> {
           int statusCode2 = putResponse.statusCode();
           System.out.println("Status - " + statusCode2 + " at " + System.currentTimeMillis() + " for " + url);
           context.assertEquals(204, statusCode2);
           //add a user
           send("http://localhost:"+port+"/users", context, HttpMethod.POST, createUserRequest,
             SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, response3 -> {
               int statusCode3 = response3.statusCode();
               System.out.println("Status - " + statusCode3 + " at " + System.currentTimeMillis() + " for " + url);
               context.assertEquals(201, statusCode3);
               //add a user to the group
               send("http://localhost:"+port+location+"/users/7261ecaae3a74dc68b468e12a70b1aec", context,
                 HttpMethod.PUT, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, response4 -> {
                   int statusCode4 = response4.statusCode();
                   System.out.println("Status - " + statusCode4 + " at " + System.currentTimeMillis() + " for " + url);
                   context.assertEquals(204, statusCode4);
                   //get all users belonging to a specific group
                   send("http://localhost:"+port+location+"/users", context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF,
                     200, response5 -> {
                       int statusCode5 = response5.statusCode();
                       System.out.println("Status - " + statusCode5 + " at " + System.currentTimeMillis() + " for " + url);
                       context.assertEquals(200, statusCode5);
                       response5.bodyHandler( bh -> {
                         System.out.println("get all users belonging to a specific group " + bh);
                       });
                       //get all groups in groups table
                       send("http://localhost:"+port+"/groups", context, HttpMethod.GET, null,
                         SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, response6 -> {
                           int statusCode6 = response6.statusCode();
                           System.out.println("Status - " + statusCode6 + " at " + System.currentTimeMillis() + " for " + url);
                           context.assertEquals(200, statusCode6);
                           response6.bodyHandler( bh -> {
                             System.out.println("get all groups in groups table " + bh);
                           });
                           //get groups belonging to a user
                           send("http://localhost:"+port+"/users/7261ecaae3a74dc68b468e12a70b1aec/groups",
                             context, HttpMethod.GET, null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, response8 -> {
                               int statusCode8 = response8.statusCode();
                               System.out.println("Status - " + statusCode8 + " at " + System.currentTimeMillis() + " for " + url);
                               context.assertEquals(200, statusCode8);
                               response8.bodyHandler( bh -> {
                                 System.out.println("get all groups for a specific user " + bh);
                               });
                               //delete a group - should fail as there is a user associated with the group
                               send("http://localhost:"+port+location, context, HttpMethod.DELETE, null,
                                 SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, responseFail -> {
                                   int statusCodeFail = responseFail.statusCode();
                                   System.out.println("Status - " + statusCodeFail + " at " + System.currentTimeMillis() + " for " + url);
                                   context.assertEquals(400, statusCodeFail);
                                   //delete all users in a group
                                   send("http://localhost:"+port+location+"/users", context, HttpMethod.DELETE, null,
                                     SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, response7 -> {
                                       int statusCode7 = response7.statusCode();
                                       System.out.println("Status - " + statusCode7 + " at " + System.currentTimeMillis() + " for " + url);
                                       context.assertEquals(204, statusCode7);
                                       //delete a group
                                       send("http://localhost:"+port+location, context, HttpMethod.DELETE, null,
                                         SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, response9 -> {
                                           int statusCode9 = response9.statusCode();
                                           System.out.println("Status - " + statusCode9 + " at " + System.currentTimeMillis() + " for " + url);
                                           context.assertEquals(204, statusCode9);
                                           async.complete();
                                       });
                                   });
                               });
                           });
                       });
                     });
                 });
             });
       });

   });
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
}
