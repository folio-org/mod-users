package org.folio.service.impl;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.ext.sql.ResultSet;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.persist.PostgresClient;

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
    Future<ResultSet> count = Future.future();

    pgClient(tenantId).select(String.format(SELECT_USAGE_COUNT, field.getRefId()), count);

    return count.map(rs -> statistic(field, rs.getResults().get(0).getInteger(0)));
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
