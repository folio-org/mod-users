package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.tools.utils.ValidationHelper.createValidationErrorMessage;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PatronBlockLimit;
import org.folio.rest.jaxrs.resource.PatronBlockLimits;
import org.folio.rest.persist.PgUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class PatronBlockLimitsAPI implements PatronBlockLimits {

  private static final String PATRON_BLOCK_LIMITS = "patron_block_limits";
  private static final String MAX_OUTSTANDING_FEEFINE_BALANCE_ID = "cf7a0d5f-a327-4ca1-aa9e-dc55ec006b8a";

  @Validate
  @Override
  public void getPatronBlockLimits(int offset, int limit, String query, String lang,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.get(PATRON_BLOCK_LIMITS, PatronBlockLimits.class,
      org.folio.rest.jaxrs.model.PatronBlockLimits.class, query, offset, limit,
      okapiHeaders, vertxContext, GetPatronBlockLimitsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void postPatronBlockLimits(String lang, PatronBlockLimit entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    Errors errors = isEntityValid(entity);
    if (errors != null) {
      asyncResultHandler.handle(succeededFuture(PatronBlockLimits.PostPatronBlockLimitsResponse
          .respond422WithApplicationJson(errors)));
      return;
    }

    PgUtil.post(PATRON_BLOCK_LIMITS, entity, okapiHeaders, vertxContext,
      PostPatronBlockLimitsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putPatronBlockLimitsByPatronBlockLimitId(String patronBlockLimitId,
    String lang, PatronBlockLimit entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    Errors errors = isEntityValid(entity);
    if (errors != null) {
      asyncResultHandler.handle(succeededFuture(
        PatronBlockLimits.PutPatronBlockLimitsByPatronBlockLimitIdResponse
          .respond422WithApplicationJson(errors)));
    }

    PgUtil.put(PATRON_BLOCK_LIMITS, entity, patronBlockLimitId, okapiHeaders,
      vertxContext, PutPatronBlockLimitsByPatronBlockLimitIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void getPatronBlockLimitsByPatronBlockLimitId(String patronBlockLimitId,
    String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.getById(PATRON_BLOCK_LIMITS, PatronBlockLimit.class, patronBlockLimitId,
      okapiHeaders, vertxContext, GetPatronBlockLimitsByPatronBlockLimitIdResponse.class,
      asyncResultHandler);
  }

  @Validate
  @Override
  public void deletePatronBlockLimitsByPatronBlockLimitId(String patronBlockLimitId,
    String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.deleteById(PATRON_BLOCK_LIMITS, patronBlockLimitId, okapiHeaders,
      vertxContext, DeletePatronBlockLimitsByPatronBlockLimitIdResponse.class,
      asyncResultHandler);
  }

  private Errors isEntityValid(PatronBlockLimit entity) {

    return MAX_OUTSTANDING_FEEFINE_BALANCE_ID.equals(entity.getConditionId())
      ? validateRangeForDoubleValueType(entity)
      : validateRangeForIntegerValueType(entity);
  }

  private Errors validateRangeForDoubleValueType(PatronBlockLimit entity) {
    Double limit = entity.getLimit();
    if (limit > 0.1 && limit < 9999.99) {
      return null;
    }
    return createValidationErrorMessage("limit", entity.getLimit().toString(),
      "A maximum balance of 0 will result in all patrons in this group being blocked; " +
        "to skip this limit, leave value set to blank");
  }

  private Errors validateRangeForIntegerValueType(PatronBlockLimit entity) {
    Double limit = entity.getLimit();
    boolean isInt = limit % 1 == 0;
    if (limit == 0 || (isInt && limit > 0 && limit < 999999)) {
      return null;
    }
    return createValidationErrorMessage("limit", entity.getLimit().toString(),
      "Must be blank or a number from 0 to 999999");
  }
}
