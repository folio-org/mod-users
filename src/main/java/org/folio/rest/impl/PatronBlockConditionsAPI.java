package org.folio.rest.impl;

import static org.apache.commons.lang.StringUtils.isBlank;
import static org.folio.rest.tools.utils.ValidationHelper.createValidationErrorMessage;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PatronBlockCondition;
import org.folio.rest.jaxrs.resource.PatronBlockConditions;
import org.folio.rest.jaxrs.resource.Proxiesfor;
import org.folio.rest.persist.PgUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class PatronBlockConditionsAPI implements PatronBlockConditions {

  private static final String PATRON_BLOCK_CONDITIONS = "patron_block_conditions";

  @Validate
  @Override
  public void getPatronBlockConditions(int offset, int limit, String query,
    String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(PATRON_BLOCK_CONDITIONS, PatronBlockCondition.class,
      org.folio.rest.jaxrs.model.PatronBlockConditions.class, query, offset, limit,
      okapiHeaders, vertxContext, GetPatronBlockConditionsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void postPatronBlockConditions(String lang, PatronBlockCondition entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    Errors errors = isEntityValid(entity, asyncResultHandler);
    if (errors != null) {
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
        PatronBlockConditions.PostPatronBlockConditionsResponse
          .respond422WithApplicationJson(errors)));
    }

    PgUtil.post(PATRON_BLOCK_CONDITIONS, entity, okapiHeaders, vertxContext,
      PostPatronBlockConditionsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putPatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, PatronBlockCondition entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    Errors errors = isEntityValid(entity, asyncResultHandler);
    if (errors != null) {
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
        PatronBlockConditions.PutPatronBlockConditionsByPatronBlockConditionIdResponse
          .respond422WithApplicationJson(errors)));
    }

    PgUtil.put(PATRON_BLOCK_CONDITIONS, entity, patronBlockConditionId, okapiHeaders,
      vertxContext, PutPatronBlockConditionsByPatronBlockConditionIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void getPatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.getById(PATRON_BLOCK_CONDITIONS, PatronBlockCondition.class, patronBlockConditionId,
      okapiHeaders, vertxContext, GetPatronBlockConditionsByPatronBlockConditionIdResponse.class,
      asyncResultHandler);
  }

  @Validate
  @Override
  public void deletePatronBlockConditionsByPatronBlockConditionId(
    String patronBlockConditionId, String lang, Map<String,
    String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.deleteById(PATRON_BLOCK_CONDITIONS, patronBlockConditionId, okapiHeaders,
      vertxContext, DeletePatronBlockConditionsByPatronBlockConditionIdResponse.class,
      asyncResultHandler);
  }

  private Errors isEntityValid(PatronBlockCondition entity, Handler<AsyncResult<Response>> asyncResultHandler) {
    Errors errors = null;
    if (isMessageBlank(entity) && isAnyFlagTrue(entity)) {
      errors = createValidationErrorMessage("message", entity.getId(),
        "Message to be displayed is a required field if one or more blocked actions selected");
      asyncResultHandler.handle(Future.succeededFuture(
        Proxiesfor.PostProxiesforResponse.respond422WithApplicationJson(errors)));
    }
    if (!isMessageBlank(entity) && !isAnyFlagTrue(entity)) {
      errors = createValidationErrorMessage("proxyFor", entity.getId(),
        "One or more blocked actions must be selected for message to be displayed to be used");
      asyncResultHandler.handle(Future.succeededFuture(
        Proxiesfor.PostProxiesforResponse.respond422WithApplicationJson(errors)));
    }
    return errors;
  }

  private boolean isMessageBlank(PatronBlockCondition entity) {
    return isBlank(entity.getAdditionalProperties().get("message").toString());
  }

  private boolean isAnyFlagTrue(PatronBlockCondition entity) {
    return entity.getBlockBorrowing() || entity.getBlockRenewals() || entity.getBlockRequests();
  }
}
