package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.tools.utils.ValidationHelper.createValidationErrorMessage;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PatronBlockLimit;
import org.folio.rest.jaxrs.resource.PatronBlockLimits;
import org.folio.rest.persist.PgUtil;

import com.google.common.collect.ImmutableList;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class PatronBlockLimitsAPI implements PatronBlockLimits {

  private static final String PATRON_BLOCK_LIMITS = "patron_block_limits";
  // IDs come from predefined data (see resources/patron-block-conditions/*)
  private static final List<String> CONDITIONS_IDS_WITH_DOUBLE_VALUE_TYPE =
    ImmutableList.of("cf7a0d5f-a327-4ca1-aa9e-dc55ec006b8a");
  private static final String VALUE_FIELD = "value";

  @Validate
  @Override
  public void getPatronBlockLimits(int offset, int limit, String query, String lang,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.get(PATRON_BLOCK_LIMITS, PatronBlockLimit.class,
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
      asyncResultHandler.handle(succeededFuture(PostPatronBlockLimitsResponse
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
      asyncResultHandler.handle(succeededFuture(PutPatronBlockLimitsByPatronBlockLimitIdResponse
          .respond422WithApplicationJson(errors)));
      return;
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
    Double limit = entity.getValue();
    if (limit == null) {
      return null;
    }
    return CONDITIONS_IDS_WITH_DOUBLE_VALUE_TYPE.contains(entity.getConditionId())
      ? validateRangeForDoubleValueType(limit)
      : validateRangeForIntegerValueType(limit);
  }

  private Errors validateRangeForDoubleValueType(Double limit) {
    if (limit >= 0.01 && limit <= 9999.99) {
      return null;
    }
    if (limit == 0.00) {
      return createValidationErrorMessage(VALUE_FIELD, limit.toString(),
        "A maximum balance of 0 will result in all patrons in this group being blocked; " +
          "to skip this limit, leave value set to blank");
    }
    return createValidationErrorMessage(VALUE_FIELD, limit.toString(),
      "Must be blank or a number from 0.01 to 9999.99");
  }

  private Errors validateRangeForIntegerValueType(Double limit) {
    boolean isInt = limit % 1 == 0;
    if (isInt && limit >= 0 && limit <= 999999) {
      return null;
    }
    return createValidationErrorMessage(VALUE_FIELD, limit.toString(),
      "Must be blank or an integer from 0 to 999999");
  }
}
