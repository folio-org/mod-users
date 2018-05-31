/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.rest.utils;


import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import static org.folio.rest.impl.UsersAPI.TABLE_NAME_USERS;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;

/**
 *
 * @author kurt
 */
public class ExpirationTool {
  
  public static void doExpiration(Vertx vertx, Context context) {
    context.runOnContext(v -> {
      //Get a list of tenants
      PostgresClient pgClient = PostgresClient.getInstance(vertx);
      String tenantQuery = "select nspname from pg_catalog.pg_namespace where nspname LIKE '%_mod_users';";
      pgClient.select(tenantQuery, reply -> {
        if(reply.succeeded()) {
          List<JsonObject> obList = reply.result().getRows();
          for(JsonObject ob : obList) {
            String tenant = ob.getString("nspname");
            Future<Integer> expireFuture = doExpirationForTenant(vertx, context, tenant);
          }
        }
      });
    });    
  }

  private static Future<Integer> doExpirationForTenant(Vertx vertx, Context context, String tenant) {
    Future<Integer> future = Future.future();
    String nowDateString =  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'").format(new Date());
    context.runOnContext(v -> {
      PostgresClient pgClient = PostgresClient.getInstance(vertx, tenant);
      String query = String.format("active == true AND expirationDate < %s", nowDateString);
      CQL2PgJSON cql2pgJson = null;
      CQLWrapper cqlWrapper = null;
      String[] fieldList = {"*"};
      try {
        cql2pgJson = new CQL2PgJSON(Arrays.asList(TABLE_NAME_USERS+".jsonb"));
        cqlWrapper = new CQLWrapper(cql2pgJson, query);
      } catch(Exception e) {
        future.fail(e.getLocalizedMessage());
        return;
      }
      pgClient.get(TABLE_NAME_USERS, User.class, fieldList, cqlWrapper, true, reply -> {
        if(reply.failed()) {
          future.fail(reply.cause());
        } else {
          List<Object> userList = (List<Object>) reply.result().getResults();
          List<Future> futureList = new ArrayList<>();
          for(Object ob : userList) {
            User user = (User)ob;
            user.setActive(Boolean.FALSE);
            Future<Void> saveFuture = saveUser(vertx, context, tenant, user);
            futureList.add(saveFuture);
          }
          CompositeFuture compositeFuture = CompositeFuture.join(futureList);
          compositeFuture.setHandler(compRes -> {
            int succeededCount = 0;
            for(Future fut : futureList) {
              if(fut.succeeded()) {
                succeededCount++;
              }
            }
            future.complete(succeededCount);
          });
        }
      });
    });
    return future;
  }
  
  private static Future<Void> saveUser(Vertx vertx, Context context, String tenant, User user) {
    Future<Void> future = Future.future();
    context.runOnContext(v -> {
      PostgresClient pgClient = PostgresClient.getInstance(vertx, tenant);
      pgClient.save(TABLE_NAME_USERS, user, saveReply -> {
        if(saveReply.failed()) {
          future.fail(saveReply.cause());
        } else {
          future.complete();
        }
      });
    });
    return future;
  }
  
}
