package org.folio.event.service;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.domain.UserType;
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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Predicate;

import static org.folio.support.UsersApiConstants.USERNAME_ALREADY_EXISTS;

public class UserTenantService {
  private static final Logger logger = LogManager.getLogger(UserTenantService.class);
  public static final String INVALID_USER_TYPE_POPULATED = "User's 'type' field should be populated with one of the allowed values: 'patron', 'staff', 'shadow'";
  public static final String USERNAME_IS_NOT_POPULATED = "In consortium mode, the staff user must have a username";
  public static final String MEMBER_USER_TENANT_SHOULD_CONTAIN_SINGLE_RECORD = "User-tenant table in member ECS tenant should contain 1 record";

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
    return fetchFirstUserTenant(conn, okapiTenantId)
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
    return fetchFirstUserTenant(conn, okapiTenantId)
      .map(res -> res.getTotalRecords() > 0);
  }

  /**
   * User-tenant table in each member ECS tenant has only single record, in this case http requests to this tenant allowed
   * (central ECS tenant contains multiple user tenant associations necessary for login, saml-login, forgot password/username functionality).
   * This method deletes  record from user-tenant table in Member ECS tenant and after this each request to that tenant will be
   * forbidden.
   *
   * @param tenantId the tenant id
   * @param vertx the vertx instance
   * @return future with true if record was deleted or false otherwise
   * @throws IllegalStateException if deleting is not possible
   */
  public Future<Boolean> deleteMemberUserTenant(String tenantId, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> fetchFirstUserTenant(conn, tenantId)
      .compose(res -> {
        if (res.getTotalRecords() != 1) {
          return Future.failedFuture(new IllegalStateException(MEMBER_USER_TENANT_SHOULD_CONTAIN_SINGLE_RECORD));
        }
        UserTenant userTenant = res.getUserTenants().get(0);
        return tenantRepository.deleteById(conn, userTenant.getId())
          .compose(isDeleted -> {
            if (Boolean.FALSE.equals(isDeleted)) {
              return Future.failedFuture(new IllegalStateException(MEMBER_USER_TENANT_SHOULD_CONTAIN_SINGLE_RECORD));
            }
            return Future.succeededFuture(Boolean.TRUE);
          });
      }));
  }

  /**
   * User-tenant table in central ECS tenant has all necessary records.
   * This method deletes all record related to a specific tenant from user-tenant table in central ECS tenant.
   *
   * @param centralTenantId the central tenant id
   * @param tenantId the tenant id
   * @param vertx the vertx instance
   * @return future with true if records were deleted or false otherwise
   */
  public Future<Boolean> deleteCentralUserTenants(String centralTenantId, String tenantId, Vertx vertx) {
    PostgresClient pgClient = pgClientFactory.apply(vertx, centralTenantId);
    return pgClient.withConn(conn -> tenantRepository.deleteUserTenants(conn, centralTenantId, tenantId));
  }

  private Future<UserTenantCollection> fetchFirstUserTenant(Conn conn, String tenantId) {
    Criterion criterion = new Criterion().setLimit(new Limit(1)).setOffset(new Offset(0));
    return tenantRepository.fetchUserTenants(conn, tenantId, criterion);
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

  public Future<Void> validateUserAcrossTenants(User entity, User userFromStorage, Map<String, String> okapiHeaders, Conn conn, Context vertxContext) {
    Predicate<User> predicate = user -> ObjectUtils.notEqual(user.getUsername(), userFromStorage.getUsername());
    return validateUserAcrossTenants(entity, okapiHeaders, conn, vertxContext, predicate);
  }

  public Future<Void> validateUserAcrossTenants(User entity, Map<String, String> okapiHeaders, Conn conn, Context vertxContext) {
    Predicate<User> predicate = user -> ObjectUtils.notEqual(UserType.SYSTEM.getTypeName(), entity.getType());
    return validateUserAcrossTenants(entity, okapiHeaders, conn, vertxContext, predicate);
  }

  /**
   * This check performing only when we are in consortium mode,
   * for common deployments we don't need to check crosstenant username uniqueness and userType.
   * For common deployments always will be return succeeded future.
   * @param entity the user
   * @param okapiHeaders okapi headers
   * @param conn connection in transaction
   * @param vertxContext The Vertx Context Object
   * @param predicate condition for name validation
   * @return succeededFuture if crosstenant username is unique and userType is populated
   */
  private Future<Void> validateUserAcrossTenants(User entity, Map<String, String> okapiHeaders, Conn conn, Context vertxContext, Predicate<User> predicate) {
    return getConsortiaCentralTenantId(conn, okapiHeaders)
      .compose(consortiaCentralTenantId -> {
        if (Objects.nonNull(consortiaCentralTenantId)) {
          logger.info("Found central tenant id = {}", consortiaCentralTenantId);
          return isUserTypePopulated(entity)
            .compose(aVoid -> {
              if (UserType.STAFF.getTypeName().equals(entity.getType()) && StringUtils.isBlank(entity.getUsername())) {
                logger.error(USERNAME_IS_NOT_POPULATED);
                return Future.failedFuture(USERNAME_IS_NOT_POPULATED);
              }
              if (predicate.test(entity)) {
                return isUsernameUniqueAcrossTenants(entity.getUsername(), consortiaCentralTenantId, okapiHeaders, vertxContext);
              }
              return Future.succeededFuture();
            });
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
    boolean isValidUserType = Arrays.stream(UserType.values())
      .map(UserType::getTypeName)
      .anyMatch(userType -> userType.equals(user.getType()));
    if (isValidUserType) {
      return Future.succeededFuture();
    } else {
      logger.error("Invalid user type {} was populated for user with id {} ", user.getType(), user.getId());
      return Future.failedFuture(INVALID_USER_TYPE_POPULATED);
    }
  }
}
