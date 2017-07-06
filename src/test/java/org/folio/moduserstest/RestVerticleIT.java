package org.folio.moduserstest;

import java.net.HttpURLConnection;
import java.sql.SQLException;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import java.util.Date;
import java.util.TimeZone;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import javax.xml.bind.DatatypeConverter;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.ISODateTimeFormat;

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
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class RestVerticleIT {

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";

  private static String postRequest = "{\"group\": \"librarianPOST\",\"desc\": \"basic lib group\"}";
  private static String putRequest = "{\"group\": \"librarianPUT\",\"desc\": \"basic lib group\"}";

  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(10);

  public static void initDatabase(TestContext context) throws SQLException {
    PostgresClient postgres = PostgresClient.getInstance(vertx);
    postgres.dropCreateDatabase("test_mod_users");

    String sql = "drop schema if exists diku_mod_users cascade;\n"
        + "drop role if exists diku_mod_users;\n";
    Async async = context.async();
    PostgresClient.getInstance(vertx).runSQLFile(sql, true, result -> {
      if (result.failed()) {
        context.fail(result.cause());
      } else if (! result.result().isEmpty()) {
        context.fail("runSQLFile failed with: " + result.result().stream().collect(Collectors.joining(" ")));
      }
      async.complete();
    });
    async.await();
  }

  @BeforeClass
  public static void setup(TestContext context) throws SQLException {
    vertx = Vertx.vertx();

    initDatabase(context);

    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("localhost", port, "diku");
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port));
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      try {
        tenantClient.post(null, res2 -> {
          async.complete();
        });
      } catch(Exception e) {
        context.fail(e);
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
                  userCollectionObject.getInteger("totalRecords") == 00) {
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
           DateFormat gmtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'");
           Date createdDate = null;
           try {
             //createdDate = DatatypeConverter.parseDateTime(userObject.getString("createdDate")).getTime();
             createdDate = new DateTime(userObject.getString("createdDate")).toDate();
           } catch(Exception e) {
             future.fail(e);
             return;
           }
           Date now = new Date();
           if(createdDate.before(now)) {
            future.complete(); 
           } else {
             future.fail("Bad value for createdDate");
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
           .end();
   return future;
 }

   private Future<Void> postAnotherUser(TestContext context) {
    Future future = Future.future();
    JsonObject userObject = new JsonObject()
            .put("username", "bobcircle")
            .put("id", "2345678")
            .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if(res.statusCode() == 201) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Error adding new user: Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end(userObject.encode());
    return future;
  }

 private Future<Void> putUserGood(TestContext context) {
   Future future = Future.future();
   JsonObject userObject = new JsonObject()
            .put("username", "bobcircle")
            .put("id", "2345678")
            .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/2345678", res -> {
      if(res.statusCode() == 204) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Error adding putting user (good): Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .end(userObject.encode());
    return future;
 }

 private Future<Void> getGoodUser(TestContext context) {
   Future future = Future.future();
   HttpClient client = vertx.createHttpClient();
   client.get(port, "localhost", "/users/2345678", res -> {
     if(res.statusCode() == 200) {
       res.bodyHandler(buf -> {
         JsonObject userObject = buf.toJsonObject();
         if(userObject.getString("username").equals("bobcircle")) {
           Date createdDate = null;
           Date updatedDate = null;
           try {
             createdDate = new DateTime(userObject.getString("createdDate")).toDate();
             updatedDate = new DateTime(userObject.getString("updatedDate")).toDate();
           } catch(Exception e) {
             future.fail(e);
             return;
           }
           Date now = new Date();
           if(createdDate.before(now) && updatedDate.before(now) && createdDate.before(updatedDate)) {
            future.complete(); 
           } else {
             future.fail("Bad value for createdDate and/or updatedDate");
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
           .end();
   return future;
 }



  private Future<Void> putUserBadUsername(TestContext context) {
   Future future = Future.future();
   JsonObject userObject = new JsonObject()
            .put("username", "joeblock")
            .put("id", "2345678")
            .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/2345678", res -> {
      if(res.statusCode() == 400) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Error adding putting user (bad username): Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .end(userObject.encode());
    return future;
 }

  private Future<Void> putUserBadId(TestContext context) {
   Future future = Future.future();
   JsonObject userObject = new JsonObject()
            .put("username", "joeblock")
            .put("id", "2345677")
            .put("active", false);
    HttpClient client = vertx.createHttpClient();
    client.put(port, "localhost", "/users/2345678", res -> {
      if(res.statusCode() == 400) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Error adding putting user (bad id): Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .end(userObject.encode());
    return future;
 }

  private Future<Void> createAddressType(TestContext context) {
    Future future = Future.future();
    JsonObject addressTypeObject = new JsonObject()
            .put("addressType", "home")
            .put("desc", "The patron's primary residence");
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/addresstypes", res -> {
      if(res.statusCode() == 201) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Error creating new addresstype Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .end(addressTypeObject.encode());
    return future;

 }

  private Future<Void> getAddressTypeUpdateUser(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port,"localhost", "/addresstypes?query=addressType=home", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(body -> {
          future.fail("Expected 200, got statusCode " + res.statusCode() + ":" + body.toString());
        });
      } else {
        res.bodyHandler(body -> {
          JsonObject result = new JsonObject(body.toString());
          JsonObject addressType = result.getJsonArray("addressTypes").getJsonObject(0);
          if(!addressType.getString("addressType").equals("home")) {
            future.fail("addressType is not 'home' in return addresstype");
          } else {
            JsonObject userObject = new JsonObject()
              .put("username", "bobcircle")
              .put("id", "2345678")
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
            putClient.put(port, "localhost", "/users/2345678", putRes -> {
              putRes.bodyHandler(putBody -> {
                if(putRes.statusCode() != 204) {
                  future.fail("Expected status 204. Got " + putRes.statusCode() + ": " + putBody.toString());
                } else {
                  HttpClient deleteClient = vertx.createHttpClient();
                  client.delete(port, "localhost", "/addresstypes/" + 
                    addressType.getString("id"), deleteRes -> {
                    deleteRes.bodyHandler(deleteBody -> {
                      if(deleteRes.statusCode() != 400) {
                        future.fail("Expected status 400, got " + deleteRes.statusCode() +
                          " : " + deleteBody.toString());
                      } else {
                        future.complete();
                      }
                    });
                  })
                    .putHeader("X-Okapi-Tenant", "diku")
                    .putHeader("content-type", "application/json")
                    .putHeader("accept", "text/plain")
                    .end();
                }
              });
            })
              .putHeader("X-Okapi-Tenant", "diku")
              .putHeader("content-type", "application/json")
              .putHeader("accept", "text/plain")
              .end(userObject.encode());
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

 private Future<Void> createAndDeleteAddressType(TestContext context) {
    Future future = Future.future();
    HttpClient postClient = vertx.createHttpClient();
    JsonObject addressTypeObject = new JsonObject()
      .put("addressType", "work")
      .put("desc", "The patron's work address");
    postClient.post(port, "localhost", "/addresstypes", postRes -> {
      postRes.bodyHandler(postBody -> {
        if(postRes.statusCode() != 201) {
          future.fail("Expected 201, got " + postRes.statusCode() + " : " +
            postBody.toString());
        } else {
          JsonObject newAddressTypeObject = new JsonObject(postBody.toString());
          HttpClient deleteClient = vertx.createHttpClient();
          deleteClient.delete(port, "localhost", "/addresstypes/" +
            newAddressTypeObject.getString("id"), deleteRes -> {
            if(deleteRes.statusCode() == 204) {
              future.complete();
            } else {
              deleteRes.bodyHandler(deleteBody -> {
                future.fail("Expected 204, got " + deleteRes.statusCode() +
                  " : " + deleteBody.toString());
              });
            }
          })    
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "text/plain")
            .end();
        }
      });
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", "application/json")
      .putHeader("accept", "text/plain")
      .end(addressTypeObject.encode());

  return future;
 }
 
 private Future<Void> postUserWithDuplicateAddressType(TestContext context) {
    Future future = Future.future();
    String addressTypeId = "4716a236-22eb-472a-9f33-d3456c9cc9d5";
    JsonObject userObject = new JsonObject()
            .put("username", "jacktriangle")
            .put("id", "3456789")
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
      if(res.statusCode() == 400) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Expected status code 400: Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end(userObject.encode());
    return future;
  }

  private Future<Void> postUserBadAddress(TestContext context) {
    Future future = Future.future();
    String addressTypeId = "1b1ad9a7-5af5-4545-b5f0-4242ba5f62c8";
    JsonObject userObject = new JsonObject()
            .put("username", "annarhombus")
            .put("id", "456789")
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
      if(res.statusCode() == 400) {
        future.complete();
      } else {
        res.bodyHandler(body -> {
          future.fail("Expected status code 400: Got status code: " + res.statusCode() + ": " + body.toString());
        });
      }
    })
            .putHeader("X-Okapi-Tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end(userObject.encode());
    return future;
  }



 @Test
  public void doSequentialTests(TestContext context) {
    Async async = context.async();
    Future<Void> startFuture;
    Future<Void> f1 = Future.future();
    getEmptyUsers(context).setHandler(f1.completer());
    startFuture = f1.compose(v -> {
      Future<Void> f = Future.future();
      postUser(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      getUser(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      postAnotherUser(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      putUserGood(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      putUserBadUsername(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      getGoodUser(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      putUserBadId(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      createAddressType(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      getAddressTypeUpdateUser(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      createAndDeleteAddressType(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      postUserWithDuplicateAddressType(context).setHandler(f.completer());
      return f;
    }).compose(v -> {
      Future<Void> f = Future.future();
      postUserBadAddress(context).setHandler(f.completer());
      return f;
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
   String userUrl = "http://localhost:"+port+"/users";

   try {
    /**add a group*/
     CompletableFuture<Response> addGroupCF = new CompletableFuture();
     String addGroupURL = url;
     send(addGroupURL, context, HttpMethod.POST, postRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addGroupCF));
     Response addGroupResponse = addGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addGroupResponse.code, HttpURLConnection.HTTP_CREATED);
     String groupID = addGroupResponse.body.getString("id");
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

     /**delete a group*/
     CompletableFuture<Response> deleteCleanCF = new CompletableFuture();
     String deleteCleanURL = url+"/"+groupID;
     send(deleteCleanURL, context, HttpMethod.DELETE, null,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, new HTTPNoBodyResponseHandler(deleteCleanCF));
     Response deleteCleanResponse = deleteCleanCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(deleteCleanResponse.code, HttpURLConnection.HTTP_NO_CONTENT);
     System.out.println(deleteCleanResponse.body +
       "\nStatus - " + deleteCleanResponse.code + " at " + System.currentTimeMillis() + " for " + deleteCleanURL);

     /**re-add a group*/
     CompletableFuture<Response> addNewGroupCF = new CompletableFuture();
     send(addGroupURL, context, HttpMethod.POST, putRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addNewGroupCF));
     Response addNewGroupResponse = addNewGroupCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addNewGroupResponse.code, HttpURLConnection.HTTP_CREATED);
     groupID = addNewGroupResponse.body.getString("id");
     System.out.println(addNewGroupResponse.body +
       "\nStatus - " + addNewGroupResponse.code + " at " + System.currentTimeMillis() + " for " + addGroupURL);

    /**add a user*/
     CompletableFuture<Response> addUserCF = new CompletableFuture();
     String addUserURL = userUrl;
     send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley", groupID).encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addUserCF));
     Response addUserResponse = addUserCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse.code, HttpURLConnection.HTTP_CREATED);
     String userID = addUserResponse.body.getString("id");
     System.out.println(addUserResponse.body +
       "\nStatus - " + addUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**add the same user name again*/
     CompletableFuture<Response> addUserCF2 = new CompletableFuture();
     send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley", groupID).encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addUserCF2));
     Response addUserResponse2 = addUserCF2.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse2.code, 422);
     System.out.println(addUserResponse2.body +
       "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**add the same user again with same id*/
     CompletableFuture<Response> addUserCF3 = new CompletableFuture();
     send(addUserURL, context, HttpMethod.POST, createUser(userID, "jhandley", groupID).encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addUserCF3));
     Response addUserResponse3 = addUserCF3.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse3.code, 422);
     System.out.println(addUserResponse3.body +
       "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**add a user again with non existant patron group*/
     CompletableFuture<Response> addUserCF4 = new CompletableFuture();
     send(addUserURL, context, HttpMethod.POST, createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 400,  new HTTPNoBodyResponseHandler(addUserCF4));
     Response addUserResponse4 = addUserCF4.get(5, TimeUnit.SECONDS);
     context.assertEquals(addUserResponse4.code, 400);
     System.out.println(addUserResponse4.body +
       "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + addUserURL);

     /**update a user again with non existant patron group*/
     CompletableFuture<Response> updateUserCF = new CompletableFuture();
     send(addUserURL+"/"+userID, context, HttpMethod.PUT, createUser(userID, "jhandley2nd", "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 400,  new HTTPNoBodyResponseHandler(updateUserCF));
     Response updateUserResponse = updateUserCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(updateUserResponse.code, 400);
     System.out.println(updateUserResponse.body +
       "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + addUserURL+"/"+userID);

     /**update a user again with existant patron group*/
     CompletableFuture<Response> updateUser2CF = new CompletableFuture();
     send(addUserURL+"/"+userID, context, HttpMethod.PUT, createUser(userID, "jhandley2nd", groupID).encode(),
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 204,  new HTTPNoBodyResponseHandler(updateUser2CF));
     Response updateUser2Response = updateUser2CF.get(5, TimeUnit.SECONDS);
     context.assertEquals(updateUser2Response.code, 204);
     System.out.println(updateUser2Response.body +
       "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + addUserURL+"/"+userID);

     /**get all users belonging to a specific group*/
     CompletableFuture<Response> getUsersInGroupCF = new CompletableFuture();
     String getUsersInGroupURL = userUrl+"?query=patronGroup=="+groupID;
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

     /**try to get via cql*/
     CompletableFuture<Response> cqlCF = new CompletableFuture();
     String cqlURL = url+"?query=group==librarianPUT";
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
   if(r.body.getInteger("totalRecords") == size){
     return true;
   }
   return false;
 }

 private static JsonObject createUser(String id, String name, String pgId) {

   JsonObject user = new JsonObject();
   if(id !=null){
     user.put("id", id);
   }
   else{
     user.put("id", UUID.randomUUID().toString());
   }
   user.put("username", name);
   user.put("patronGroup", pgId);
   return user;
 }

}

