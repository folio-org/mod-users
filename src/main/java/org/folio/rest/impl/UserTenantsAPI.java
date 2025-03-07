package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
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
import static org.folio.repository.UserTenantRepository.BARCODE;
import static org.folio.repository.UserTenantRepository.EMAIL;
import static org.folio.repository.UserTenantRepository.EXTERNAL_SYSTEM_ID;
import static org.folio.repository.UserTenantRepository.MOBILE_PHONE_NUMBER;
import static org.folio.repository.UserTenantRepository.PHONE_NUMBER;
import static org.folio.repository.UserTenantRepository.TENANT_ID_FIELD;
import static org.folio.repository.UserTenantRepository.LOWERCASE_WRAPPED_USERNAME;
import static org.folio.repository.UserTenantRepository.USER_ID_FIELD;

public class UserTenantsAPI implements UserTenants {
  private static final Logger logger = LogManager.getLogger(UserTenantsAPI.class);

  private final UserTenantService userTenantService;

  public UserTenantsAPI() {
    this.userTenantService = new UserTenantService();
  }

  @Override
  public void getUserTenants(String userId, String username, String tenantId, String email, String phoneNumber, String mobilePhoneNumber,
                             String barcode, String externalSystemId, String queryOp, String totalRecords, int offset, int limit,
                             Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                             Context vertxContext) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    Criterion criterion = new Criterion().setLimit(new Limit(limit)).setOffset(new Offset(offset));
    ArgumentsHolder argumentsHolder = new ArgumentsHolder(userId, username, tenantId, email, phoneNumber, mobilePhoneNumber, barcode, externalSystemId);
    addWhereClauseArgumentsToCriterion(argumentsHolder, queryOp, criterion);
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
  public void postUserTenants(UserTenant userTenant, Map<String, String> okapiHeaders,
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

  @Override
  public void deleteUserTenants(String optionalTenantId, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    deleteUserTenants(okapiTenantId, optionalTenantId, vertxContext.owner())
      .onSuccess(res -> {
        logger.info("Record(s) from user_tenant has been deleted successfully. For a member tenant, all http requests to it will be forbidden");
        asyncResultHandler.handle(succeededFuture(Response.status(204).build()));
      })
      .onFailure(cause -> {
        logger.error("Could not delete user_tenant record(s)", cause);
        asyncResultHandler.handle(succeededFuture(PostUserTenantsResponse.respond500WithTextPlain(cause.getMessage())));
      });
  }

  /**
   * This method is used to delete user-tenant records from member tenant or central tenant table for two scenarios during ECS tenant deletion: <br/>
   * 1. When we soft-delete a tenant, we need to delete a single record from the member tenant's user-tenant table. <br/>
   * 2. When we hard-delete a tenant, we need to delete all records for this tenant from central tenant table. <br/>
   * <br/>
   * The <code>optionalTenantId</code> is an optional parameter, if it is not provided, then we assume that request was sent to the member tenant,
   * and we need to delete a single record from user-tenant table, otherwise we need to delete all records for this tenant from central tenant table
   *
   * @param tenantId tenant id from request headers
   * @param optionalTenantId optional tenant id
   * @param vertx vertx instance
   * @return future with boolean result
   */
  private Future<Boolean> deleteUserTenants(String tenantId, String optionalTenantId, Vertx vertx) {
    return StringUtils.isBlank(optionalTenantId)
      ? userTenantService.deleteMemberUserTenant(tenantId, vertx)
      : userTenantService.deleteCentralUserTenants(tenantId, tenantId, vertx);
  }

  private void addWhereClauseArgumentsToCriterion(ArgumentsHolder argumentsHolder, String queryOp, Criterion criterion) {
    Map<String, String> fields = Map.of(
        USER_ID_FIELD, StringUtils.defaultString(argumentsHolder.userId()),
        LOWERCASE_WRAPPED_USERNAME, StringUtils.defaultString(StringUtils.toRootLowerCase(argumentsHolder.username())),
        TENANT_ID_FIELD, StringUtils.defaultString(argumentsHolder.tenantId()),
        EMAIL, StringUtils.defaultString(argumentsHolder.email()),
        PHONE_NUMBER, StringUtils.defaultString(argumentsHolder.phoneNumber()),
        MOBILE_PHONE_NUMBER, StringUtils.defaultString(argumentsHolder.mobilePhoneNumber()),
        BARCODE, StringUtils.defaultString(argumentsHolder.barcode()),
        EXTERNAL_SYSTEM_ID, StringUtils.defaultString(argumentsHolder.externalSystemId())
      );

    fields.entrySet().stream()
      .filter(entry -> StringUtils.isNotBlank(entry.getValue()))
      .map(entry -> new Criteria().addField(entry.getKey()).setOperation("=").setVal(entry.getValue()).setJSONB(false))
      .forEach(param -> criterion.addCriterion(param, queryOp));
  }

  record ArgumentsHolder(
    String userId,
    String username,
    String tenantId,
    String email,
    String phoneNumber,
    String mobilePhoneNumber,
    String barcode,
    String externalSystemId
  ) {
  }
}
