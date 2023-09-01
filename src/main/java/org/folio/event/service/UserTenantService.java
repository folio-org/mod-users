package org.folio.event.service;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.repository.UserTenantRepository;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.OkapiConnectionParams;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

import static org.folio.rest.impl.UsersAPI.USERNAME_ALREADY_EXISTS;

public class UserTenantService {
  private static final Logger logger = LogManager.getLogger(UserTenantService.class);
  public static final String USER_TYPE_NOT_POPULATED = "The user type was not populated to a user with an id ";

  private final UserTenantRepository tenantRepository;
  private final BiFunction<Vertx, String, PostgresClient> pgClientFactory;

  public UserTenantService() {
    pgClientFactory = PostgresClient::getInstance;
    tenantRepository = new UserTenantRepository();
  }

  public Future<UserTenantCollection> fetchUserTenants(String tenantId, Criterion criterion, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> tenantRepository.fetchUserTenants(conn, tenantId, criterion));
  }

  /**
   * Get the consortia central tenant id, if we are not in consortium mode - we always return null result and it is expected
   * @param conn connection in transaction
   * @param okapiHeaders okapi headers
   * @return For consortia return - consortiaCentralTenantId, for common deployment - null
   */
  public Future<String> getConsortiaCentralTenantId(Conn conn, Map<String, String> okapiHeaders) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    Criterion criterion = new Criterion().setLimit(new Limit(1)).setOffset(new Offset(0));
    return tenantRepository.fetchUserTenants(conn, okapiTenantId, criterion)
      .map(res -> {
        if (res.getTotalRecords() > 0) {
          return res.getUserTenants().stream().map(UserTenant::getCentralTenantId).findFirst().orElse(null);
        }
        return null;
      });
  }

  /**
   * Check is it consortia tenant or common deployment.
   * @param conn connection in transaction
   * @param okapiHeaders okapi headers
   * @return succeededFuture(true) if it's consortia tenant
   */
  public Future<Boolean> isConsortiaTenant(Conn conn, Map<String, String> okapiHeaders) {
    String okapiTenantId = TenantTool.tenantId(okapiHeaders);
    Criterion criterion = new Criterion().setLimit(new Limit(1)).setOffset(new Offset(0));
    return tenantRepository.fetchUserTenants(conn, okapiTenantId, criterion)
      .map(res -> res.getTotalRecords() > 0);
  }

  public Future<Boolean> saveUserTenant(UserTenant userTenant, String tenantId, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> tenantRepository.saveUserTenant(conn, userTenant, tenantId));
  }

  public Future<Boolean> updateUserTenant(UserTenant userTenant, String tenantId, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> tenantRepository.updateUserTenant(conn, userTenant, tenantId));
  }

  public Future<Boolean> deleteUserTenant(UserTenant userTenant, String tenantId, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> tenantRepository.deleteUserTenant(conn, userTenant, tenantId));
  }

  /**
   * This check performing only when we are in consortium mode,
   * for common deployments we don't need to check crosstenant username uniqueness and userType.
   * For common deployments always will be return succeeded future.
   * @param entity the user
   * @param userFromStorage the user from storage
   * @param okapiHeaders okapi headers
   * @param conn connection in transaction
   * @param vertxContext The Vertx Context Object
   * @return succeededFuture if crosstenant username is unique and userType is populated
   */
  public Future<Void> validateUserAcrossTenants(User entity, User userFromStorage, Map<String, String> okapiHeaders, Conn conn, Context vertxContext) {
    return getConsortiaCentralTenantId(conn, okapiHeaders)
      .compose(consortiaCentralTenantId -> {
        if (Objects.nonNull(consortiaCentralTenantId)) {
          logger.info("Found central tenant id = {}", consortiaCentralTenantId);
          return isUserTypePopulated(entity)
            .compose(aVoid -> {
              if (ObjectUtils.notEqual(entity.getUsername(), userFromStorage.getUsername())) {
                return isUsernameUniqueAcrossTenants(entity.getUsername(), consortiaCentralTenantId, okapiHeaders, vertxContext);
              }
              return Future.succeededFuture();
            });
        }
        return Future.succeededFuture();
      });
  }

  /**
   * This check performing only when we are in consortium mode,
   * for common deployments we don't need to check crosstenant username uniqueness and userType.
   * For common deployments always will be return succeeded future.
   * @param entity the user
   * @param okapiHeaders okapi headers
   * @param conn connection in transaction
   * @param vertxContext The Vertx Context Object
   * @return succeededFuture if crosstenant username is unique and userType is populated
   */
  public Future<Void> validateUserAcrossTenants(User entity, Map<String, String> okapiHeaders, Conn conn, Context vertxContext) {
    return getConsortiaCentralTenantId(conn, okapiHeaders)
      .compose(consortiaCentralTenantId -> {
        if (Objects.nonNull(consortiaCentralTenantId)) {
          logger.info("Found central tenant id = {}", consortiaCentralTenantId);
          return isUserTypePopulated(entity)
            .compose(aVoid -> isUsernameUniqueAcrossTenants(entity.getUsername(), consortiaCentralTenantId, okapiHeaders, vertxContext));
        }
        return Future.succeededFuture();
      });
  }

  private Future<Void> isUsernameUniqueAcrossTenants(String username, String consortiaCentralTenantId, Map<String, String> okapiHeaders, Context vertxContext) {
    Map<String, String> okapiHeadersForCentralTenant = new HashMap<>(okapiHeaders);
    okapiHeadersForCentralTenant.put(OkapiConnectionParams.OKAPI_TENANT_HEADER, consortiaCentralTenantId);
    okapiHeadersForCentralTenant.put(RestVerticle.OKAPI_HEADER_TENANT, consortiaCentralTenantId);
    PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeadersForCentralTenant);
    return postgresClient.withConn(conn -> tenantRepository.isUsernameAlreadyExists(conn, username, consortiaCentralTenantId)
      .compose(isUsernameAlreadyExists -> {
        if (Boolean.TRUE.equals(isUsernameAlreadyExists)) {
          logger.error("User with this username {} already exists", username);
          String errorMessage = String.format("User with this username %s already exists. Error code: %s", username, USERNAME_ALREADY_EXISTS);
          return Future.failedFuture(errorMessage);
        }
        return Future.succeededFuture();
      }));
  }

  private Future<Void> isUserTypePopulated(User user) {
    if (Objects.isNull(user.getType())) {
      String errorMessage = USER_TYPE_NOT_POPULATED + user.getId();
      logger.error(errorMessage);
      return Future.failedFuture(errorMessage);
    }
    return Future.succeededFuture();
  }
}
