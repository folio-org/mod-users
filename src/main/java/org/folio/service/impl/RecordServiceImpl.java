package org.folio.service.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.List;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.apache.commons.collections4.CollectionUtils;

import org.folio.model.RecordUpdate;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldOptionStatistic;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.User;
import org.folio.service.RecordService;

public class RecordServiceImpl implements RecordService {

  private static final Logger LOG = LoggerFactory.getLogger(RecordServiceImpl.class);

  private final RecordRepository repository;

  public RecordServiceImpl(Vertx vertx) {
    this.repository = new RecordRepositoryImpl(vertx);
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatistic(CustomField field, String tenantId) {
    LOG.info("Retrieving custom field statistic: field = {}", field);

    return repository.retrieveStatisticForField(field, tenantId);
  }

  @Override
  public Future<CustomFieldOptionStatistic> retrieveOptionStatistic(CustomField field, String optId, String tenantId) {
    LOG.info("Retrieving custom field option statistic: field = {}, option ID = {}", field, optId);

    return repository.retrieveStatisticForFieldOption(field, optId, tenantId);
  }

  @Override
  public Future<Void> deleteAllValues(CustomField field, String tenantId) {
    LOG.info("Removing custom field values from user records: field = {}", field);

    Future<List<User>> related = repository.findUsersByField(field, tenantId)
      .onSuccess(users -> LOG.info("The number of users found with the given field: {}", users.size()));

    return related.compose(users -> removeCustomFieldFromUsers(users, field, tenantId));
  }

  @Override
  public Future<Void> deleteMissedOptionValues(RecordUpdate recordUpdate, String tenantId) {
    LOG.info("Removing custom field values from user records: refId = {}", recordUpdate.getRefId());

    return repository.findUsersByFieldValues(recordUpdate, tenantId)
      .onSuccess(users -> LOG.info("The number of users found with the given field: {}", users.size()))
      .compose(users -> removeCustomFieldValueOrSetDefault(users, recordUpdate, tenantId));
  }

  private Future<Void> removeCustomFieldFromUsers(List<User> users, CustomField field, String tenantId) {
    Future<Void> updated = succeededFuture();

    for (User user : users) {
      Object removedValue = user.getCustomFields().getAdditionalProperties().remove(field.getRefId());

      updated = updated
        .compose(v -> repository.updateUser(user, tenantId))
        .map(found -> {
          LOG.debug("Field removed from user: refId = {}, value = {}, userName = {}",
            field.getRefId(), removedValue, user.getUsername());
          return null;
        });
    }

    return updated;
  }

  private Future<Void> removeCustomFieldValueOrSetDefault(List<User> users, RecordUpdate recordUpdate, String tenantId) {
    Future<Void> updated = succeededFuture();
    for (User user : users) {
      CustomFields customFields = user.getCustomFields();
      Object value = customFields.getAdditionalProperties().get(recordUpdate.getRefId());
      if (value instanceof List) {
        @SuppressWarnings("unchecked")
        List<String> values = (List<String>) value;
        updateListValue(customFields, recordUpdate, values);
      } else {
        updateStringValue(recordUpdate, customFields);
      }

      updated = updated
        .compose(v -> repository.updateUser(user, tenantId))
        .map(found -> {
          LOG.debug("Field update on user: recordUpdate = {}, userName = {}", recordUpdate, user.getUsername());
          return null;
        });
    }
    return updated;
  }

  private void updateStringValue(RecordUpdate recordUpdate, CustomFields customFields) {
    String refId = recordUpdate.getRefId();
    List<String> defaultIds = recordUpdate.getDefaultIds();
    if (CollectionUtils.isEmpty(defaultIds)) {
      customFields.getAdditionalProperties().remove(refId);
    } else if (defaultIds.size() == 1) {
      customFields.getAdditionalProperties().replace(refId, defaultIds.get(0));
    } else {
      customFields.getAdditionalProperties().replace(refId, defaultIds);
    }
  }

  private void updateListValue(CustomFields customFields, RecordUpdate recordUpdate, List<String> values) {
    String refId = recordUpdate.getRefId();
    List<String> optionIdsToDelete = recordUpdate.getOptionIdsToDelete();
    List<String> defaultIds = recordUpdate.getDefaultIds();

    values.removeAll(optionIdsToDelete);
    if (values.isEmpty() && !defaultIds.isEmpty()) {
      values.addAll(defaultIds);
    }
    if (values.isEmpty()) {
      customFields.getAdditionalProperties().remove(refId);
    } else if (values.size() == 1) {
      customFields.setAdditionalProperty(refId, values.get(0));
    } else {
      customFields.setAdditionalProperty(refId, values);
    }
  }

}
