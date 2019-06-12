package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.jaxrs.model.ProxyforCollection;
import org.folio.rest.jaxrs.resource.Proxiesfor;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.utils.PostgresClientUtil;
import org.folio.rest.utils.ValidationHelper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSONException;

/**
 *
 * @author kurt
 */
public class ProxiesForAPI implements Proxiesfor {

  public static final String PROXY_FOR_TABLE = "proxyfor";
  public static final String ID_FIELD_NAME = "id";
  public static final String USERID_FIELD_NAME = "'userId'";
  public static final String PROXY_USERID_FIELD_NAME = "'proxyUserId'";
  public static final String URL_PREFIX = "/proxiesfor";
  private static final Logger logger = LoggerFactory.getLogger(ProxiesForAPI.class);
  private boolean suppressErrorResponse = false;

  public void setSuppressErrorResponse(boolean suppressErrorResponse) {
    this.suppressErrorResponse = suppressErrorResponse;
  }

  private String getErrorResponse(String response) {
    if (suppressErrorResponse) {
      return "Internal Server Error: Please contact Admin";
    }
    return response;
  }

  private String logAndSaveError(Throwable err) {
    String message = err.getLocalizedMessage();
    logger.error(message, err);
    return message;
  }

  @Override
  public void getProxiesfor(String query,
    int offset, int limit,
    String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.get(PROXY_FOR_TABLE, ProxiesFor.class, ProxyforCollection.class,
      query, offset, limit, okapiHeaders, vertxContext,
      GetProxiesforResponse.class, asyncResultHandler);
  }

  @Override
  public void postProxiesfor(
    String lang,
    ProxiesFor entity,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    try {
      PostgresClient postgresClient = PostgresClientUtil.getInstance(vertxContext, okapiHeaders);
      userAndProxyUserComboExists(entity.getUserId(), entity.getProxyUserId(),
        postgresClient).setHandler(existsRes -> {
          if (existsRes.failed()) {
            String message = logAndSaveError(existsRes.cause());
            asyncResultHandler.handle(Future.succeededFuture(
              PostProxiesforResponse.respond500WithTextPlain(
                getErrorResponse(message))));
          } else if (existsRes.result() == true) {
            Errors existsError = ValidationHelper.createValidationErrorMessage(
              "proxyFor", entity.getId(), "Proxy relationship already exists");
            asyncResultHandler.handle(Future.succeededFuture(
              PostProxiesforResponse.respond422WithApplicationJson(existsError)));
          } else {
            try {
              String id = entity.getId();
              if (id == null) {
                id = UUID.randomUUID().toString();
                entity.setId(id);
              }
              PgUtil.post(PROXY_FOR_TABLE, entity, okapiHeaders, vertxContext,
                PostProxiesforResponse.class, asyncResultHandler);
            } catch (Exception e) {
              String message = logAndSaveError(e);
              asyncResultHandler.handle(Future.succeededFuture(
                PostProxiesforResponse.respond500WithTextPlain(
                  getErrorResponse(message))));
            }
          }
        });
    } catch (Exception e) {
      String message = logAndSaveError(e);
      asyncResultHandler.handle(Future.succeededFuture(
        PostProxiesforResponse.respond500WithTextPlain(
          getErrorResponse(message))));
    }
  }

  @Override
  public void getProxiesforById(String id,
    String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.getById(PROXY_FOR_TABLE, ProxiesFor.class, id, okapiHeaders,
      vertxContext, GetProxiesforByIdResponse.class, asyncResultHandler);
  }

  @Override
  public void deleteProxiesforById(String id,
    String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.deleteById(PROXY_FOR_TABLE, id, okapiHeaders, vertxContext,
      DeleteProxiesforByIdResponse.class, asyncResultHandler);
  }

  @Override
  public void putProxiesforById(String id,
    String lang,
    ProxiesFor entity,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.put(PROXY_FOR_TABLE, entity, id, okapiHeaders, vertxContext, PutProxiesforByIdResponse.class, asyncResultHandler);
  }

  private Future<Boolean> userAndProxyUserComboExists(
    String userId,
    String proxyUserId,
    PostgresClient postgresClient) {
    Future<Boolean> future = Future.future();
    try {
      Criteria userCrit = new Criteria().addField(USERID_FIELD_NAME).
        setOperation("=").setValue("'" + userId + "'").setJSONB(true);
      Criteria proxyUserCrit = new Criteria().addField(PROXY_USERID_FIELD_NAME).
        setOperation("=").setValue("'" + proxyUserId + "'").setJSONB(true);
      Criterion criterion = new Criterion();
      criterion.addCriterion(userCrit, "AND", proxyUserCrit);
      postgresClient.get(PROXY_FOR_TABLE, ProxiesFor.class, criterion, true, getReply -> {
        try {
          if (getReply.failed()) {
            future.fail(getReply.cause());
          } else {
            List<ProxiesFor> proxyForList = getReply.result().getResults();
            if (proxyForList.isEmpty()) {
              future.complete(false);
            } else {
              future.complete(true);
            }
          }
        } catch (Exception e) {
          future.fail(e);
        }
      });
    } catch (Exception e) {
      future.fail(e);
    }
    return future;
  }

}
