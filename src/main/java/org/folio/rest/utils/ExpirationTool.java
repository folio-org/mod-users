package org.folio.rest.utils;


import static java.time.format.DateTimeFormatter.ISO_INSTANT;
import static org.folio.rest.impl.UsersAPI.TABLE_NAME_USERS;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Vertx;

public final class ExpirationTool {
  private static final Logger logger = LogManager.getLogger(ExpirationTool.class);

  BiFunction<Vertx, String, PostgresClient> postgresClientFactory;

  public ExpirationTool() {
    this(PostgresClient::getInstance);
  }

  public ExpirationTool(BiFunction<Vertx, String, PostgresClient> postgresClientFactory) {
    this.postgresClientFactory = postgresClientFactory;
  }

  public Future<Integer> doExpirationForTenant(Vertx vertx, String tenant) {
    try {
      String nowDateString = ZonedDateTime.now().format(ISO_INSTANT);
      String query = String.format("active == true AND expirationDate < %s", nowDateString);
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(List.of(TABLE_NAME_USERS+".jsonb"));
      CQLWrapper cqlWrapper = new CQLWrapper(cql2pgJson, query);
      String[] fieldList = {"*"};

      if (StringUtils.isEmpty(tenant)) {
        return Future.failedFuture(
          new IllegalArgumentException("Cannot expire users for undefined tenant"));
      }

      PostgresClient pgClient = postgresClientFactory.apply(vertx, tenant);

      return pgClient.get(TABLE_NAME_USERS, User.class, fieldList, cqlWrapper, true)
        .onFailure(error ->
          logger.error(String.format("Error executing postgres query for tenant %s: '%s', %s",
            tenant, query, error.getMessage()), error))
        .compose(results -> disableUsers(vertx, tenant, query, results));
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      return Future.failedFuture(e);
    }
  }

  private Future<Integer> disableUsers(Vertx vertx, String tenant,
    String query, Results<User> results) {

    if (results.getResults().isEmpty()) {
      logger.debug("No results found for tenant {} and query {}", tenant, query);
      return Future.succeededFuture(0);
    }

    final var userList = results.getResults();
    List<Future<Void>> futureList = new ArrayList<>();

    for (User user : userList) {
      futureList.add(disableUser(vertx, tenant, user));
    }
    CompositeFuture compositeFuture = GenericCompositeFuture.join(futureList);

    return compositeFuture.compose(compRes -> {
      int succeededCount = 0;
      for (Future<Void> fut : futureList) {
        if (fut.succeeded()) {
          succeededCount++;
        }
      }

      return Future.succeededFuture(succeededCount);
    });
  }

  Future<Void> disableUser(Vertx vertx, String tenant, User user) {
    logger.info("Disabling expired user with id {} for tenant {}", user.getId(), tenant);

    user.setActive(Boolean.FALSE);

    try {
      PostgresClient pgClient = postgresClientFactory.apply(vertx, tenant);

      return pgClient.update(TABLE_NAME_USERS, user, user.getId())
        .onFailure(cause -> logger.error(String.format(
          "Error updating user %s for tenant %s: %s", user.getId(), tenant,
          cause.getMessage()), cause))
        .mapEmpty();

    } catch(Exception e) {
      return Future.failedFuture(e);
    }
  }
}
