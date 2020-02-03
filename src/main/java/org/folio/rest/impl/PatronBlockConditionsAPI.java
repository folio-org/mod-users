package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.jaxrs.model.PatronBlockCondition;
import org.folio.rest.jaxrs.resource.PatronBlockConditions;
import org.folio.rest.persist.PgUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class PatronBlockConditionsAPI implements PatronBlockConditions {

  private static final String PATRON_BLOCK_CONDITIONS = "patron_block_conditions";

  @Override
  public void getPatronBlockConditions(int offset, int limit, String query,
    String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(PATRON_BLOCK_CONDITIONS, PatronBlockCondition.class,
      org.folio.rest.jaxrs.model.PatronBlockConditions.class, query, offset, limit,
      okapiHeaders, vertxContext, GetPatronBlockConditionsResponse.class, asyncResultHandler);
  }

  @Override
  public void postPatronBlockConditions(String lang, PatronBlockCondition entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.post(PATRON_BLOCK_CONDITIONS, entity, okapiHeaders, vertxContext,
      PostPatronBlockConditionsResponse.class, asyncResultHandler);
  }

  @Override
  public void putPatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, PatronBlockCondition entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.put(PATRON_BLOCK_CONDITIONS, entity, patronBlockConditionId, okapiHeaders,
      vertxContext, PutPatronBlockConditionsByPatronBlockConditionIdResponse.class, asyncResultHandler);
  }

  @Override
  public void getPatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.getById(PATRON_BLOCK_CONDITIONS, PatronBlockCondition.class, patronBlockConditionId,
      okapiHeaders, vertxContext, GetPatronBlockConditionsByPatronBlockConditionIdResponse.class,
      asyncResultHandler);
  }

  @Override
  public void deletePatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, Map<String,
    String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.deleteById(PATRON_BLOCK_CONDITIONS, patronBlockConditionId, okapiHeaders,
      vertxContext, DeletePatronBlockConditionsByPatronBlockConditionIdResponse.class,
      asyncResultHandler);
  }
}
