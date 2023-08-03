package org.folio.event.service;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.repository.UserTenantRepository;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;

import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

import static org.folio.rest.impl.UsersAPI.USERNAME_ALREADY_EXISTS;

public class UserTenantService {

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

  public Future<Void> isUsernameUpdatedAndUniqueAcrossTenants(User entity, Map<String, String> okapiHeaders, Conn conn, User userFromStorage) {
    return ObjectUtils.notEqual(entity.getUsername(), userFromStorage.getUsername()) ? isUsernameUniqueAcrossTenants(entity, okapiHeaders, conn) : Future.succeededFuture();
  }

  public Future<Void> isUsernameUniqueAcrossTenants(User entity, Map<String, String> okapiHeaders, Conn conn) {
    return getConsortiaCentralTenantId(conn, okapiHeaders)
      .compose(consortiaCentralTenantId -> {
        if (Objects.nonNull(consortiaCentralTenantId)) {
          return isUsernameUniqueAcrossTenants(conn, entity.getUsername(), consortiaCentralTenantId);
        }
        return Future.succeededFuture();
      });
  }

  public Future<Void> isUsernameUniqueAcrossTenants(Conn conn, String username, String consortiaCentralTenantId) {
    return tenantRepository.isUsernameAlreadyExists(conn, username, consortiaCentralTenantId)
      .compose(isUsernameAlreadyExists -> {
        if (isUsernameAlreadyExists) {
          String errorMessage = String.format("User with this username %s already exists. Error code: %s", username, USERNAME_ALREADY_EXISTS);
          return Future.failedFuture(errorMessage);
        }
        return Future.succeededFuture();
      });
  }
}
