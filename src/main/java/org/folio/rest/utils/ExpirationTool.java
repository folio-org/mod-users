package org.folio.rest.utils;


import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import static org.folio.rest.impl.UsersAPI.TABLE_NAME_USERS;
import static org.folio.rest.impl.UsersAPI.RAML_PATH;
import static org.folio.rest.impl.UsersAPI.USER_ID_FIELD;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;

/**
 *
 * @author kurt
 */
public class ExpirationTool {

  private ExpirationTool() {
    //do nothing
  }


  public static void doExpiration(Vertx vertx, Context context) {
    final Logger logger = LoggerFactory.getLogger(ExpirationTool.class);
    logger.info("Calling doExpiration()");
    context.runOnContext(v -> {
      //Get a list of tenants
      PostgresClient pgClient = PostgresClient.getInstance(vertx);
      String tenantQuery = "select nspname from pg_catalog.pg_namespace where nspname LIKE '%_mod_users';";
      pgClient.select(tenantQuery, reply -> {
        if(reply.succeeded()) {
          List<JsonObject> obList = reply.result().getRows();
          for(JsonObject ob : obList) {
            String nsTenant = ob.getString("nspname");
            String suffix = "_mod_users";
            int suffixLength = nsTenant.length() - suffix.length();
            final String tenant = nsTenant.substring(0, suffixLength);
            logger.info("Calling doExpirationForTenant for tenant " + tenant);
            Future<Integer> expireFuture = doExpirationForTenant(vertx, context, tenant);
            expireFuture.setHandler(res -> {
              if(res.failed()) {
                logger.info(String.format("Attempt to expire records for tenant %s failed: %s",
                        tenant, res.cause().getLocalizedMessage()));
              } else {
                logger.info(String.format("Expired %s users", res.result()));
              }
            });
          }
        } else {
          logger.info(String.format("TenantQuery '%s' failed: %s", tenantQuery,
                  reply.cause().getLocalizedMessage()));
        }
      });
    });
  }

  public static Future<Integer> doExpirationForTenant(Vertx vertx, Context context, String tenant) {
    final Logger logger = LoggerFactory.getLogger(ExpirationTool.class);
    Future<Integer> future = Future.future();
    String nowDateString =  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS\'Z\'").format(new Date());
    context.runOnContext(v -> {
      PostgresClient pgClient = PostgresClient.getInstance(vertx, tenant);
      pgClient.setIdField("id");
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
      pgClient.get(TABLE_NAME_USERS, User.class, fieldList, cqlWrapper, true, false, reply -> {
        if(reply.failed()) {
          logger.info(String.format("Error executing postgres query: '%s', %s",
            query, reply.cause().getLocalizedMessage()));
          future.fail(reply.cause());
        } else if(reply.result().getResults().isEmpty()) {
          logger.info(String.format("No results found for query %s", query));
        } else {
          List<User> userList = reply.result().getResults();
          List<Future> futureList = new ArrayList<>();
          for(User user : userList) {
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
    final Logger logger = LoggerFactory.getLogger(ExpirationTool.class);
    logger.info(String.format("Updating user with id %s", user.getId()));
    Future<Void> future = Future.future();
    context.runOnContext(v -> {
      try {
        PostgresClient pgClient = PostgresClient.getInstance(vertx, tenant);
        pgClient.setIdField("id");
        Criteria idCrit = new Criteria(RAML_PATH + "/userdata.json");
        idCrit.addField(USER_ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(user.getId());
        Criterion criterion = new Criterion(idCrit);

        pgClient.update(TABLE_NAME_USERS, user, criterion, true, updateReply -> {
          if(updateReply.failed()) {
            logger.info(String.format("Error updating user %s: %s", user.getId(),
              updateReply.cause().getLocalizedMessage()));
            future.fail(updateReply.cause());
          } else {
            future.complete();
          }
        });
      } catch(Exception e) {
        future.tryFail(e);
      }
    });
    return future;
  }

}
