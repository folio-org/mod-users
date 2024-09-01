package org.folio.event.service;

import java.util.Map;
import java.util.function.BiFunction;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.service.UsersService;

import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class UserUpdateService {
  private static final Logger logger = LogManager.getLogger(UserTenantService.class);
  private final BiFunction<Vertx, String, PostgresClient> pgClientFactory;

  public UserUpdateService() {
    pgClientFactory = PostgresClient::getInstance;
  }

  public Future<User> updateUser(User oldEntity, User newEntity, String tenantId, Vertx vertx,
    Map<String, String> headers) {
    logger.info("updateUser:: Updating user with id: {}", newEntity.getId());
    if (oldEntity.getBarcode() != null && oldEntity.getBarcode().equals(newEntity.getBarcode())
      && oldEntity.getPatronGroup() != null && oldEntity.getPatronGroup().equals(newEntity.getPatronGroup())) {
      logger.info("updateUser:: barcode and patronGroup not changed for user with id: {}",
        newEntity.getId());
      return Future.succeededFuture(newEntity);
    }
    UsersService usersService = new UsersService(vertx.getOrCreateContext(), headers);
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> usersService.updateUser(conn, oldEntity, newEntity, true));
  }
}
