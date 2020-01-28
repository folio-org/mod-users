package org.folio.service.impl;

import java.util.Date;
import java.util.List;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.ext.sql.ResultSet;
import io.vertx.ext.sql.UpdateResult;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.interfaces.Results;

public class RecordRepositoryImpl implements RecordRepository {

  private static final String USERS_TABLE = "users";
  private static final String SELECT_USAGE_COUNT =
    "SELECT count(*)" +
    "  FROM " + USERS_TABLE +
    "  WHERE jsonb -> 'customFields' ->> '%s' IS NOT NULL";

  private Vertx vertx;


  public RecordRepositoryImpl(Vertx vertx) {
    this.vertx = vertx;
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatisticForField(CustomField field, String tenantId) {
    Promise<ResultSet> count = Promise.promise();

    pgClient(tenantId).select(String.format(SELECT_USAGE_COUNT, field.getRefId()), count);

    return count.future().map(rs -> statistic(field, rs.getResults().get(0).getInteger(0)));
  }

  @Override
  public Future<List<User>> findUsersByField(CustomField field, String tenantId) {
    Criterion criterion = new Criterion(
      new Criteria().addField("'customFields'").addField("'" + field.getRefId() + "'").setOperation("IS NOT NULL"));

    Promise<Results<User>> promise = Promise.promise();

    pgClient(tenantId).get(USERS_TABLE, User.class, criterion, false, promise);

    return promise.future().map(Results::getResults);
  }

  @Override
  public Future<Boolean> updateUser(User user, String tenantId) {
    user.setUpdatedDate(new Date());

    Promise<UpdateResult> promise = Promise.promise();

    pgClient(tenantId).update(USERS_TABLE, user, user.getId(), promise);

    return promise.future().map(updateResult -> updateResult.getUpdated() == 1);
  }

  private CustomFieldStatistic statistic(CustomField field, Integer count) {
    return new CustomFieldStatistic()
      .withFieldId(field.getId())
      .withEntityType(field.getEntityType())
      .withCount(count);
  }

  private PostgresClient pgClient(String tenantId) {
    return PostgresClient.getInstance(vertx, tenantId);
  }

}
