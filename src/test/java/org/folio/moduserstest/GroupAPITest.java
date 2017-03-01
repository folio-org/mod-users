package org.folio.moduserstest;

import io.vertx.core.DeploymentOptions;
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

import java.io.IOException;
import java.util.ArrayList;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.AdminClient;
import org.folio.rest.client.TenantClient;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author shale
 *
 */

@RunWith(VertxUnitRunner.class)
public class GroupAPITest {

  private static Vertx      vertx;
  private ArrayList<String> urls;

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";

  private static String postRequest = "{\"group\": \"librarianPOST\",\"desc\": \"basic lib group\"}";
  private static String putRequest = "{\"group\": \"librarianPUT\",\"desc\": \"basic lib group\"}";
  private static String createUserRequest = "{ \"username\": \"jhandey\" , \"id\": \"7261ecaae3a74dc68b468e12a70b1aec\"}";


  static int port;
  static TenantClient tClient = null;
  static AdminClient aClient  = null;

  /*
  POST http://localhost:8083/groups
  GET http://localhost:8083/groups?query=group==librariansa
  GET http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d
  DELETE http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d
  DELETE http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d (with users in the groups table)
  PUT http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d
  GET http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d/users
  DELETE http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d/users
  PUT http://localhost:8083/groups/d0faefc6-68c0-4612-8ee2-8aeaf058349d/users/09b67590-c8d0-420a-b149-1cfeed76482d
  GET http://localhost:8083/users/{userId}/groups
  */

  @Before
  public void setUp(TestContext context) throws IOException {
    vertx = Vertx.vertx();

    try {
      setupPostgres();
    } catch (Exception e) {
      e.printStackTrace();
    }

    Async async = context.async();

    port = NetworkUtils.nextFreePort();

    aClient = new AdminClient("localhost", port, "harvard");
    tClient = new TenantClient("localhost", port, "harvard");

    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port",
      port));
    vertx.deployVerticle(RestVerticle.class.getName(), options, context.asyncAssertSuccess(id -> {
      try {
        tClient.post(null, response -> {
          response.bodyHandler( body -> {
            System.out.println(body.toString());
            async.complete();
          });
        });

      } catch (Exception e) {
        e.printStackTrace();
      }
    }));

  }

  private static void setupPostgres() throws Exception {
    PostgresClient.setIsEmbedded(true);
    PostgresClient.setEmbeddedPort(NetworkUtils.nextFreePort());
    PostgresClient.getInstance(vertx).startEmbeddedPostgres();
  }

  @After
  public void tearDown(TestContext context) {
    PostgresClient.stopEmbeddedPostgres();

/*    Async async = context.async();
    tClient.delete( reply -> {
      reply.bodyHandler( body2 -> {
        System.out.println(body2.toString());
        vertx.close(context.asyncAssertSuccess( res-> {
          PostgresClient.stopEmbeddedPostgres();
          async.complete();
        }));
      });
    });*/

  }

/*  private void handleResponse(TestContext context, HttpClientResponse response, String url, Async async){
    int statusCode = response.statusCode();
    System.out.println("Status - " + statusCode + " at " + System.currentTimeMillis() + " for " + url);
    context.assertInRange(200, statusCode, 4);
    async.complete();
  }

  @Test
  public void postGroup(TestContext context){
    Async async = context.async();
    String url = "http://localhost:"+port+"/groups";
    send(url, context, HttpMethod.POST, postRequest,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 201, response -> {
        handleResponse(context, response, url, async);
    });
  }

  @Test
  public void getGroups(TestContext context){
    Async async = context.async();
    String url = "http://localhost:"+port+"/groups";
    send(url, context, HttpMethod.GET, null,
      SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, response -> {
        handleResponse(context, response, url, async);
    });
  }
*/
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
    request.putHeader("Authorization", "harvard");
    request.putHeader("x-okapi-tenant", "harvard");
    request.putHeader("Accept", "application/json,text/plain");
    request.putHeader("Content-type", contentType);
    request.end(buffer);
  }

}
