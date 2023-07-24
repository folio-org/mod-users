package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.event.service.UserTenantService;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.resource.UserTenants;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.tools.utils.TenantTool;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.repository.UserTenantRepository.EMAIL;
import static org.folio.repository.UserTenantRepository.MOBILE_PHONE_NUMBER;
import static org.folio.repository.UserTenantRepository.PHONE_NUMBER;
import static org.folio.repository.UserTenantRepository.TENANT_ID_FIELD;
import static org.folio.repository.UserTenantRepository.USERNAME_FIELD;
import static org.folio.repository.UserTenantRepository.USER_ID_FIELD;

public class UserTenantsAPI implements UserTenants {
  private static final Logger logger = LogManager.getLogger(UserTenantsAPI.class);

  private final UserTenantService userTenantService;

  public UserTenantsAPI() {
    this.userTenantService = new UserTenantService();
  }

  @Override
  public void getUserTenants(String userId, String username, String tenantId, String email, String phoneNumber, String mobilePhoneNumber, String queryOp, int offset, int limit, String lang,
                             Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                             Context vertxContext) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    Criterion criterion = new Criterion().setLimit(new Limit(limit)).setOffset(new Offset(offset));
    addWhereClauseArgumentsToCriterion(userId, username, tenantId, email, phoneNumber, mobilePhoneNumber, queryOp, criterion);
    logger.debug("Trying to get user-tenant records with criterion: {}.", criterion);

    userTenantService.fetchUserTenants(okapiTenantId, criterion, vertxContext.owner())
      .onSuccess(res -> {
        logger.debug("Number of existing user-tenant records: {}.", res.getTotalRecords());
        asyncResultHandler.handle(succeededFuture(GetUserTenantsResponse.respond200WithApplicationJson(res)));
      })
      .onFailure(cause -> {
        logger.error("Could not get user-tenant records", cause);
        asyncResultHandler.handle(succeededFuture(GetUserTenantsResponse.respond500WithTextPlain(cause.getMessage())));
      });
  }

  @Override
  public void postUserTenants(String lang, UserTenant userTenant, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    logger.debug("Trying to save user-tenant with id: {}, userId: {}, userName: {}, tenantId: {}.",
      userTenant.getId(), userTenant.getUserId(), userTenant.getUsername(), userTenant.getTenantId());

    userTenantService.saveUserTenant(userTenant, okapiTenantId, vertxContext.owner())
      .onSuccess(res -> {
        logger.info("user-tenant with id: {}, userId: {}, userName: {}, tenantId: {} has been saved successfully.",
          userTenant.getId(), userTenant.getUserId(), userTenant.getUsername(), userTenant.getTenantId());
        asyncResultHandler.handle(succeededFuture(Response.status(201).build()));
      })
      .onFailure(cause -> {
        logger.error("Could not save user-tenant record", cause);
        asyncResultHandler.handle(succeededFuture(PostUserTenantsResponse.respond500WithTextPlain(cause.getMessage())));
      });
  }

  private void addWhereClauseArgumentsToCriterion(String userId, String username, String tenantId, String email,
                                                  String phoneNumber, String mobilePhoneNumber, String queryOp, Criterion criterion) {
    Map<String, String> fields = Map.of(
      USER_ID_FIELD, StringUtils.defaultString(userId),
      USERNAME_FIELD, StringUtils.defaultString(username),
      TENANT_ID_FIELD, StringUtils.defaultString(tenantId),
      EMAIL, StringUtils.defaultString(email),
      PHONE_NUMBER, StringUtils.defaultString(phoneNumber),
      MOBILE_PHONE_NUMBER, StringUtils.defaultString(mobilePhoneNumber)
      );

    fields.entrySet().stream()
      .filter(entry -> StringUtils.isNotBlank(entry.getValue()))
      .map(entry -> new Criteria().addField(entry.getKey()).setOperation("=").setVal(entry.getValue()).setJSONB(false))
      .forEach(param -> criterion.addCriterion(param, queryOp));
  }
}
