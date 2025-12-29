package org.folio.service.impl;

import static org.folio.rest.utils.ResultHandlerUtils.getAsyncResultHandler;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.folio.model.RecordUpdate;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;

public class RecordRepositoryImpl implements RecordRepository {

  private static final String USERS_TABLE = "users";
  private static final String NOT_NULL_CONDITION = "IS NOT NULL";

  private final Vertx vertx;

  public RecordRepositoryImpl(Vertx vertx) {
    this.vertx = vertx;
  }

  @Override
  public Future<List<User>> findUsersByField(CustomField field, String tenantId) {
    Criteria criteria = new Criteria()
      .addField("'customFields'")
      .addField("'" + field.getRefId() + "'")
      .setOperation(NOT_NULL_CONDITION);

    Promise<Results<User>> promise = Promise.promise();

    pgClient(tenantId).get(USERS_TABLE, User.class, new Criterion(criteria), false, getAsyncResultHandler(promise));

    return promise.future().map(Results::getResults);
  }

  @Override
  public Future<List<User>> findUsersByFieldValues(RecordUpdate recordUpdate, String tenantId) {
    String joinedIds = recordUpdate.getOptionIdsToDelete().stream()
      .map(opt -> "'" + opt + "'")
      .collect(Collectors.joining(", "));
    String path = "jsonb -> 'customFields' -> '" + recordUpdate.getRefId() + "'";
    CQLWrapper filter = new CQLWrapper().setWhereClause("WHERE " + path + " ?| ARRAY[" + joinedIds + "]");
    Promise<Results<User>> promise = Promise.promise();
    pgClient(tenantId).get(USERS_TABLE, User.class, filter, false, getAsyncResultHandler(promise));
    return promise.future().map(Results::getResults);
  }

  @Override
  public Future<Void> updateUser(User user, String tenantId) {
    user.setUpdatedDate(new Date());

    Promise<RowSet<Row>> promise = Promise.promise();

    pgClient(tenantId).update(USERS_TABLE, user, user.getId(), getAsyncResultHandler(promise));

    return promise.future().mapEmpty();
  }

  private PostgresClient pgClient(String tenantId) {
    return PostgresClient.getInstance(vertx, tenantId);
  }

}
