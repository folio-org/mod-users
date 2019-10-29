package org.folio.service.impl;

import static io.vertx.core.Future.succeededFuture;

import io.vertx.core.Future;
import io.vertx.core.Vertx;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.service.RecordService;

public class RecordServiceImpl implements RecordService {

  private RecordRepository repository;


  public RecordServiceImpl(Vertx vertx) {
    this.repository = new RecordRepositoryImpl(vertx);
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatistic(CustomField field, String tenantId) {
    return repository.retrieveStatisticForField(field, tenantId);
  }

  @Override
  public Future<Void> deleteAllValues(CustomField field, String tenantId) {
    return succeededFuture();
  }

}
