package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.List;
import java.util.Map;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.jaxrs.model.ProxyforCollection;
import org.folio.rest.jaxrs.resource.Proxiesfor;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.ValidationHelper;

/**
 *
 * @author kurt
 */
public class ProxiesForAPI implements Proxiesfor {

  public static final String PROXY_FOR_TABLE = "proxyfor";
  public static final String USERID_FIELD_NAME = "'userId'";
  public static final String PROXY_USERID_FIELD_NAME = "'proxyUserId'";
  private static final Logger logger = LoggerFactory.getLogger(ProxiesForAPI.class);
  private boolean suppressErrorResponse = false;

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
    PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
    userAndProxyUserComboExists(entity.getUserId(), entity.getProxyUserId(),
      postgresClient).setHandler(existsRes -> {
        if (existsRes.failed()) {
          String message = logAndSaveError(existsRes.cause());
          asyncResultHandler.handle(Future.succeededFuture(
            PostProxiesforResponse.respond500WithTextPlain(
              getErrorResponse(message))));
          return;
        }
        if (Boolean.TRUE.equals(existsRes.result())) {
          Errors existsError = ValidationHelper.createValidationErrorMessage(
            "proxyFor", entity.getId(), "Proxy relationship already exists");
          asyncResultHandler.handle(Future.succeededFuture(
            PostProxiesforResponse.respond422WithApplicationJson(existsError)));
          return;
        }
        PgUtil.post(PROXY_FOR_TABLE, entity, okapiHeaders, vertxContext,
          PostProxiesforResponse.class, asyncResultHandler);
      });

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

  Future<Boolean> userAndProxyUserComboExists(
    String userId,
    String proxyUserId,
    PostgresClient postgresClient) {
    Promise<Boolean> promise = Promise.promise();
    Criteria userCrit = new Criteria().addField(USERID_FIELD_NAME).
      setOperation("=").setVal(userId).setJSONB(true);
    Criteria proxyUserCrit = new Criteria().addField(PROXY_USERID_FIELD_NAME).
      setOperation("=").setVal(proxyUserId).setJSONB(true);
    Criterion criterion = new Criterion();
    criterion.addCriterion(userCrit, "AND", proxyUserCrit);
    postgresClient.get(PROXY_FOR_TABLE, ProxiesFor.class, criterion, true, getReply -> {
      if (getReply.failed()) {
        promise.fail(getReply.cause());
      } else {
        List<ProxiesFor> proxyForList = getReply.result().getResults();
        promise.complete(! proxyForList.isEmpty());
      }
    });
    return promise.future();
  }
}
