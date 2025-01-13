package org.folio.rest.utils;


import static java.time.format.DateTimeFormatter.ISO_INSTANT;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.service.event.EntityChangedEventPublisherFactory.userEventPublisher;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

import io.vertx.core.Context;
import io.vertx.core.json.Json;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;

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

  public Future<Integer> doExpirationForTenant(Context vertxContext, Map<String, String> okapiHeaders) {
    try {
      String tenant = okapiHeaders.get(OKAPI_TENANT_HEADER);
      String nowDateString = ZonedDateTime.now().format(ISO_INSTANT);
      String query = String.format("active == true AND expirationDate < %s", nowDateString);
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(List.of(TABLE_NAME_USERS+".jsonb"));
      CQLWrapper cqlWrapper = new CQLWrapper(cql2pgJson, query);
      String[] fieldList = {"*"};

      if (StringUtils.isEmpty(tenant)) {
        return Future.failedFuture(
          new IllegalArgumentException("Cannot expire users for undefined tenant"));
      }

      PostgresClient pgClient = postgresClientFactory.apply(vertxContext.owner(), tenant);

      return pgClient.get(TABLE_NAME_USERS, User.class, fieldList, cqlWrapper, true)
        .onFailure(error ->
          logger.error(String.format("Error executing postgres query for tenant %s: '%s', %s",
            tenant, query, error.getMessage()), error))
        .compose(results -> disableUsers(vertxContext, tenant, query, results, okapiHeaders));
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      return Future.failedFuture(e);
    }
  }

  private Future<Integer> disableUsers(Context vertxContext, String tenant,
                                       String query, Results<User> results, Map<String, String> okapiHeaders) {

    if (results.getResults().isEmpty()) {
      logger.debug("No results found for tenant {} and query {}", tenant, query);
      return Future.succeededFuture(0);
    }

    List<Future<Void>> futureList = new ArrayList<>();

    results.getResults()
      .forEach(user -> futureList.add(disableUser(vertxContext, tenant, user, okapiHeaders)));

    return GenericCompositeFuture.join(futureList)
      .map(compRes -> futureList.stream()
        .filter(Future::succeeded)
        .mapToInt(a -> 1)
        .sum());
  }

  Future<Void> disableUser(Context vertxContext, String tenant, User user, Map<String, String> okapiHeaders) {
    logger.info("Disabling expired user with id {} for tenant {}", user.getId(), tenant);
    User oldUserEntity = Json.decodeValue(Json.encode(user), User.class);
    user.setActive(Boolean.FALSE);

    try {
      PostgresClient pgClient = postgresClientFactory.apply(vertxContext.owner(), tenant);

      return pgClient.update(TABLE_NAME_USERS, user, user.getId())
              .onSuccess(hander -> userEventPublisher(vertxContext, okapiHeaders)
                      .publishUpdated(user.getId(), oldUserEntity, user))
        .onFailure(cause -> logger.error(String.format(
          "Error updating user %s for tenant %s: %s", user.getId(), tenant,
          cause.getMessage()), cause))
        .mapEmpty();

    } catch(Exception e) {
      return Future.failedFuture(e);
    }
  }
}
