package org.folio.event.service;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;
import java.util.function.BiFunction;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.PostgresClient;
import org.folio.service.UsersService;

import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class UserUpdateService {
  private static final Logger logger = LogManager.getLogger(UserUpdateService.class);
  private final BiFunction<Vertx, String, PostgresClient> pgClientFactory;

  public UserUpdateService() {
    pgClientFactory = PostgresClient::getInstance;
  }

  public Future<User> updateUser(User newEntity, String tenantId, Vertx vertx,
    Map<String, String> headers) {
    logger.info("updateUser:: Updating user with id: {}", newEntity.getId());
    UsersService usersService = new UsersService(vertx.getOrCreateContext(), headers);
    PostgresClient pgClient = pgClientFactory.apply(vertx, tenantId);
    return pgClient.withConn(conn -> usersService.getUserByIdForUpdate(conn, newEntity.getId()).compose(userFromStorage -> {
      if (userFromStorage == null) {
        logger.error("updateUser:: User with id: {} not found", newEntity.getId());
        return Future.succeededFuture(newEntity);
      }
      boolean userUpdated = false;
      if (userFromStorage.getBarcode() == null || !userFromStorage.getBarcode().equals(newEntity.getBarcode())) {
        userFromStorage.setBarcode(newEntity.getBarcode());
        userUpdated = true;
      }
      if (userFromStorage.getPatronGroup() == null || !userFromStorage.getPatronGroup().equals(newEntity.getPatronGroup())) {
        userFromStorage.setPatronGroup(newEntity.getPatronGroup());
        userUpdated = true;
      }
      if (!userUpdated) {
        logger.info("updateUser:: barcode and patronGroup not changed for user with id: {}",
          newEntity.getId());
        return Future.succeededFuture(userFromStorage);
      } else {
        userFromStorage.setBarcode(newEntity.getBarcode());
        userFromStorage.setPatronGroup(newEntity.getPatronGroup());
        return usersService.updateUser(conn, userFromStorage, userFromStorage, true);
      }
    }));
  }
}
