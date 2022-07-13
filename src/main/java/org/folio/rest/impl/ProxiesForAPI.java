package org.folio.rest.impl;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.jaxrs.model.ProxyforCollection;
import org.folio.rest.jaxrs.resource.Proxiesfor;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.ValidationHelper;
import org.folio.support.FailureHandler;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class ProxiesForAPI implements Proxiesfor {
  public static final String PROXY_FOR_TABLE = "proxyfor";
  public static final String USERID_FIELD_NAME = "'userId'";
  public static final String PROXY_USERID_FIELD_NAME = "'proxyUserId'";
  private static final Logger logger = LogManager.getLogger(ProxiesForAPI.class);

  @Override
  public void getProxiesfor(String query, int offset, int limit, String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(PROXY_FOR_TABLE, ProxiesFor.class, ProxyforCollection.class,
      query, offset, limit, okapiHeaders, vertxContext,
      GetProxiesforResponse.class, asyncResultHandler);
  }

  @Override
  public void postProxiesfor(String lang, ProxiesFor entity,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    var postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostProxiesforResponse::respond500WithTextPlain);

    userAndProxyUserComboExists(entity, postgresClient)
      .onSuccess(proxyAlreadyExists -> {
        if (Boolean.TRUE.equals(proxyAlreadyExists)) {
          logger.error("Proxy relationship already exists: {}", entity.getId());

          Errors existsError = ValidationHelper.createValidationErrorMessage(
            "proxyFor", entity.getId(), "Proxy relationship already exists");

          asyncResultHandler.handle(Future.succeededFuture(
            PostProxiesforResponse.respond422WithApplicationJson(existsError)));
          return;
        }

        PgUtil.post(PROXY_FOR_TABLE, entity, okapiHeaders, vertxContext,
        PostProxiesforResponse.class, asyncResultHandler);
      })
      .onFailure(failureHandler::handleFailure);
  }

  @Override
  public void getProxiesforById(String id, String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.getById(PROXY_FOR_TABLE, ProxiesFor.class, id, okapiHeaders,
      vertxContext, GetProxiesforByIdResponse.class, asyncResultHandler);
  }

  @Override
  public void deleteProxiesforById(String id, String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.deleteById(PROXY_FOR_TABLE, id, okapiHeaders, vertxContext,
      DeleteProxiesforByIdResponse.class, asyncResultHandler);
  }

  @Override
  public void putProxiesforById(String id, String lang, ProxiesFor entity,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.put(PROXY_FOR_TABLE, entity, id, okapiHeaders, vertxContext,
      PutProxiesforByIdResponse.class, asyncResultHandler);
  }

  Future<Boolean> userAndProxyUserComboExists(ProxiesFor proxyRelationship,
    PostgresClient postgresClient) {

    Criteria userCrit = new Criteria().addField(USERID_FIELD_NAME).
      setOperation("=").setVal(proxyRelationship.getUserId()).setJSONB(true);

    Criteria proxyUserCrit = new Criteria().addField(PROXY_USERID_FIELD_NAME).
      setOperation("=").setVal(proxyRelationship.getProxyUserId()).setJSONB(true);

    Criterion criterion = new Criterion();
    criterion.addCriterion(userCrit, "AND", proxyUserCrit);

    return postgresClient.get(PROXY_FOR_TABLE, ProxiesFor.class, criterion, true)
      .map(results -> {
        List<ProxiesFor> proxyForList = results.getResults();
        return !proxyForList.isEmpty();
      });
  }
}
