package org.folio.service.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.model.RecordUpdate;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldOptionStatistic;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Conn;
import org.folio.service.RecordService;

import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class RecordServiceImpl implements RecordService {

  private static final Logger LOG = LogManager.getLogger(RecordServiceImpl.class);

  private final RecordService delegate;
  private final RecordRepository repository;

  public RecordServiceImpl(Vertx vertx) {
    var entityTableMap = Map.of("user", TABLE_NAME_USERS);
    this.repository = new RecordRepositoryImpl(vertx);
    this.delegate = org.folio.service.RecordServiceImpl.createForSingleTable(vertx, entityTableMap);
  }

  @Override
  public Future<CustomFieldStatistic> retrieveStatistic(CustomField field, String tenantId) {
    LOG.info("Retrieving custom field statistic: field = {}", field);

    return delegate.retrieveStatistic(field, tenantId);
  }

  @Override
  public Future<CustomFieldOptionStatistic> retrieveOptionStatistic(CustomField field, String optId, String tenantId) {
    LOG.info("Retrieving custom field option statistic: field = {}, option ID = {}", field, optId);

    return delegate.retrieveOptionStatistic(field, optId, tenantId);
  }

  @Override
  public Future<Void> deleteAllValues(CustomField field, String tenantId) {
    LOG.info("Removing custom field values from user records: field = {}", field);

    repository.findUsersByField(field, tenantId)
      .onSuccess(RecordServiceImpl::logFoundUsersCount);

    return delegate.deleteAllValues(field, tenantId);
  }

  @Override
  public Future<Void> deleteAllValues(Conn conn, CustomField customField, String s) {
    return delegate.deleteAllValues(conn, customField, s);
  }

  @Override
  public Future<Void> deleteMissedOptionValues(RecordUpdate recordUpdate, String tenantId) {
    LOG.info("Removing custom field values from user records: refId = {}", recordUpdate.getRefId());

    return repository.findUsersByFieldValues(recordUpdate, tenantId)
      .onSuccess(RecordServiceImpl::logFoundUsersCount)
      .compose(users -> removeCustomFieldValueOrSetDefault(users, recordUpdate, tenantId));
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
      customFields.getAdditionalProperties().replace(refId, defaultIds.getFirst());
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
      customFields.setAdditionalProperty(refId, values.getFirst());
    } else {
      customFields.setAdditionalProperty(refId, values);
    }
  }

  private static void logFoundUsersCount(List<User> users) {
    LOG.info("The number of users found with the given field: {}", users.size());
  }

}
