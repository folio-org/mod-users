package org.folio.service.impl;

import static io.vertx.core.Future.failedFuture;

import java.util.List;

import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.rest.impl.UsersAPI;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;

import io.vertx.core.Future;

public class UserRepository {
  private final PostgresClient postgresClient;

  public UserRepository(PostgresClient postgresClient) {
    this.postgresClient = postgresClient;
  }

  public Future<Boolean> addressTypeAssignedToUser(String addressTypeId,
    CQLWrapperSupplier cqlSupplier) {

    final var query = "personal.addresses=" + addressTypeId;
    final CQLWrapper cql;

    try {
      cql = cqlSupplier.supply(query, 1, 0);
    } catch (CQL2PgJSONException e) {
      return failedFuture(e);
    }

    return postgresClient.get(UsersAPI.TABLE_NAME_USERS, User.class, new String[]{"*"},
      cql, true, false, List.of())
      .map(Results::getResults)
      .map(users -> !users.isEmpty());
  }

  public interface CQLWrapperSupplier {
    CQLWrapper supply(String cql, int limit, int offset) throws CQL2PgJSONException;
  }
}
