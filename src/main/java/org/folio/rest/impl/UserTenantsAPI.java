package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.apache.commons.lang3.StringUtils;
import org.folio.event.service.UserTenantService;
import org.folio.rest.jaxrs.resource.UserTenants;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.tools.utils.TenantTool;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.repository.UserTenantRepository.USER_ID_FIELD;
import static org.folio.repository.UserTenantRepository.USERNAME_FIELD;
import static org.folio.repository.UserTenantRepository.TENANT_ID_FIELD;

public class UserTenantsAPI implements UserTenants {

  private final UserTenantService userTenantService;

  public UserTenantsAPI() {
    this.userTenantService = new UserTenantService();
  }

  @Override
  public void getUserTenants(String userId, String username, String tenantId, int offset, int limit, String lang,
                             Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                             Context vertxContext) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    Criterion criterion = new Criterion().setLimit(new Limit(limit)).setOffset(new Offset(offset));
    addWhereClauseArgumentsToCriterion(userId, username, tenantId, criterion);

    userTenantService.fetchUserTenants(okapiTenantId, criterion, vertxContext.owner())
      .onSuccess(res -> asyncResultHandler.handle(
        succeededFuture(GetUserTenantsResponse.respond200WithApplicationJson(res))))
      .onFailure(cause -> asyncResultHandler.handle(
        succeededFuture(GetUserTenantsResponse.respond500WithTextPlain(cause.getMessage()))));
  }

  private void addWhereClauseArgumentsToCriterion(String userId, String username, String tenantId, Criterion criterion) {
    Map<String, String> fields = Map.of(
      USER_ID_FIELD, StringUtils.defaultString(userId),
      USERNAME_FIELD, StringUtils.defaultString(username),
      TENANT_ID_FIELD, StringUtils.defaultString(tenantId)
    );

    fields.entrySet().stream()
      .filter(entry -> StringUtils.isNotBlank(entry.getValue()))
      .map(entry -> new Criteria().addField(entry.getKey()).setOperation("=").setVal(entry.getValue()).setJSONB(false))
      .forEach(criterion::addCriterion);
  }
}
