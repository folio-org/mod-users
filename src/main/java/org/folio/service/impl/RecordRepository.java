package org.folio.service.impl;

import io.vertx.core.Future;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;

public interface RecordRepository {

  Future<CustomFieldStatistic> retrieveStatisticForField(CustomField field, String tenantId);

}
