package org.folio.service.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.List;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.User;
import org.folio.service.RecordService;

public class RecordServiceImpl implements RecordService {

  private static final Logger logger = LoggerFactory.getLogger(RecordServiceImpl.class);

  private RecordRepository repository;


  public RecordServiceImpl(Vertx vertx) {
    this.repository = new RecordRepositoryImpl(vertx);
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatistic(CustomField field, String tenantId) {
    logger.info("Retrieving custom field statistic: field = {0}", field);

    return repository.retrieveStatisticForField(field, tenantId);
  }

  @Override
  public Future<Void> deleteAllValues(CustomField field, String tenantId) {
    logger.info("Removing custom field values from user records: field = {0}", field);

    Future<List<User>> related = repository.findUsersByField(field, tenantId)
      .onSuccess(users -> logger.info("The number of users found with the given field: {0}", users.size()));

    return related.compose(users -> removeCustomFieldFromUsers(users, field, tenantId));
  }

  private Future<Void> removeCustomFieldFromUsers(List<User> users, CustomField field, String tenantId) {
    Future<Void> updated = succeededFuture();

    for (User user : users) {
      Object removedValue = user.getCustomFields().getAdditionalProperties().remove(field.getRefId());

      updated = updated
        .compose(v -> repository.updateUser(user, tenantId))
        .map(found -> {
          logger.debug("Field removed from user: refId = {0}, value = {1}, userName = {2}",
            field.getRefId(), removedValue, user.getUsername());
          return null;
        });
    }

    return updated;
  }

}
