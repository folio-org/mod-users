package org.folio.service.impl;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;

import org.folio.model.RecordUpdate;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldOptionStatistic;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;

public class RecordRepositoryImpl implements RecordRepository {

  private static final String USERS_TABLE = "users";
  private static final String NOT_NULL_CONDITION = "IS NOT NULL";
  private static final String EXIST_CONDITION = "? $2";
  private static final String SELECT_USAGE_COUNT =
    "SELECT count(*)" +
      "  FROM " + USERS_TABLE +
      "  WHERE jsonb -> 'customFields' -> $1 ";

  private final Vertx vertx;

  public RecordRepositoryImpl(Vertx vertx) {
    this.vertx = vertx;
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatisticForField(CustomField field, String tenantId) {
    Promise<RowSet<Row>> count = Promise.promise();

    Tuple params = Tuple.of(field.getRefId());
    pgClient(tenantId).select(SELECT_USAGE_COUNT + NOT_NULL_CONDITION, params, count);

    return count.future().map(rs -> fieldStatistic(field, rs.iterator().next().getInteger(0)));
  }

  @Override
  public Future<CustomFieldOptionStatistic> retrieveStatisticForFieldOption(CustomField field, String optId,
                                                                            String tenantId) {
    Promise<RowSet<Row>> count = Promise.promise();

    Tuple params = Tuple.of(field.getRefId(), optId);
    pgClient(tenantId).select(SELECT_USAGE_COUNT + EXIST_CONDITION, params, count);

    return count.future().map(rs -> fieldOptionStatistic(field, optId, rs.iterator().next().getInteger(0)));
  }

  @Override
  public Future<List<User>> findUsersByField(CustomField field, String tenantId) {
    Criteria criteria = new Criteria()
      .addField("'customFields'")
      .addField("'" + field.getRefId() + "'")
      .setOperation(NOT_NULL_CONDITION);

    Promise<Results<User>> promise = Promise.promise();

    pgClient(tenantId).get(USERS_TABLE, User.class, new Criterion(criteria), false, promise);

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
    pgClient(tenantId).get(USERS_TABLE, User.class, filter, false, promise);
    return promise.future().map(Results::getResults);
  }

  @Override
  public Future<Boolean> updateUser(User user, String tenantId) {
    user.setUpdatedDate(new Date());

    Promise<RowSet<Row>> promise = Promise.promise();

    pgClient(tenantId).update(USERS_TABLE, user, user.getId(), promise);

    return promise.future().map(updateResult -> updateResult.size() == 1);
  }

  private CustomFieldStatistic fieldStatistic(CustomField field, Integer count) {
    return new CustomFieldStatistic()
      .withFieldId(field.getId())
      .withEntityType(field.getEntityType())
      .withCount(count);
  }

  private CustomFieldOptionStatistic fieldOptionStatistic(CustomField field, String optId, Integer count) {
    return new CustomFieldOptionStatistic()
      .withOptionId(optId)
      .withCustomFieldId(field.getId())
      .withEntityType(field.getEntityType())
      .withCount(count);
  }

  private PostgresClient pgClient(String tenantId) {
    return PostgresClient.getInstance(vertx, tenantId);
  }

}
