package org.folio.rest.utils;


import static org.folio.rest.impl.UsersAPI.TABLE_NAME_USERS;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.function.BiFunction;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;

public final class ExpirationTool {
  private static final Logger logger = LogManager.getLogger(ExpirationTool.class);
  /** PostgresClient::getInstance, or some other method for unit testing */
  static BiFunction<Vertx, String, PostgresClient> postgresClient = PostgresClient::getInstance;

  private ExpirationTool() {
    throw new UnsupportedOperationException("Cannot instantiate utility class.");
  }

  public static Future<Integer> doExpirationForTenant(Vertx vertx, String tenant) {
    Promise<Integer> promise = Promise.promise();
    try {
      String nowDateString =  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").format(new Date());
      String query = String.format("active == true AND expirationDate < %s", nowDateString);
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(List.of(TABLE_NAME_USERS+".jsonb"));
      CQLWrapper cqlWrapper = new CQLWrapper(cql2pgJson, query);
      String[] fieldList = {"*"};
      PostgresClient pgClient = postgresClient.apply(vertx, tenant);
      pgClient.get(TABLE_NAME_USERS, User.class, fieldList, cqlWrapper, true, false, reply -> {
        if (reply.failed()) {
          logger.error(String.format("Error executing postgres query for tenant %s: '%s', %s",
            tenant, query, reply.cause().getMessage()), reply.cause());
          promise.fail(reply.cause());
          return;
        }
        if (reply.result().getResults().isEmpty()) {
          logger.debug(String.format("No results found for tenant %s and query %s", tenant, query));
          promise.complete(0);
          return;
        }
        List<User> userList = reply.result().getResults();
        List<Future<Void>> futureList = new ArrayList<>();
        for(User user : userList) {
          futureList.add(disableUser(vertx, tenant, user));
        }
        CompositeFuture compositeFuture = GenericCompositeFuture.join(futureList);
        compositeFuture.onComplete(compRes -> {
          int succeededCount = 0;
          for(Future<Void> fut : futureList) {
            if(fut.succeeded()) {
              succeededCount++;
            }
          }
          promise.complete(succeededCount);
        });
      });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.tryFail(e);
    }
    return promise.future();
  }

  static Future<Void> disableUser(Vertx vertx, String tenant, User user) {
    logger.info("Disabling expired user with id {} for tenant {}", user.getId(), tenant);
    user.setActive(Boolean.FALSE);
    Promise<Void> promise = Promise.promise();
    try {
      PostgresClient pgClient = postgresClient.apply(vertx, tenant);
      pgClient.update(TABLE_NAME_USERS, user, user.getId(), updateReply -> {
        if (updateReply.succeeded()) {
          promise.complete();
          return;
        }
        logger.error(String.format("Error updating user %s for tenant %s: %s", user.getId(), tenant,
          updateReply.cause().getMessage()), updateReply.cause());
        promise.fail(updateReply.cause());
      });
    } catch(Exception e) {
      promise.tryFail(e);
    }
    return promise.future();
  }

}
