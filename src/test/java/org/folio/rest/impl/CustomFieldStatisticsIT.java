package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

import java.util.List;

import org.junit.jupiter.api.Test;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
class CustomFieldStatisticsIT extends CustomFieldTestBase {

  @Test
  void shouldReturnZeroFieldUsageIfNoValuesAssigned() {
    CustomField textField = createTextField();

    CustomFieldStatistic stat = customFieldsClient.getCustomFieldStats(textField.getId());

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(0));
  }

  @Test
  void shouldReturnFieldUsageCountIfValuesAssigned() {
    CustomField textField = createTextField();
    assignValue(testUser, textField.getRefId(), "someValue");

    CustomFieldStatistic stat = customFieldsClient.getCustomFieldStats(textField.getId());

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(1));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  void shouldFailIfFieldDoesntExist() {
    String fakeFieldId = "11111111-1111-1111-a111-111111111111";
    customFieldsClient.attemptGetCustomFieldStats(fakeFieldId).statusCode(SC_NOT_FOUND);
  }

  @Test
  void shouldReturnZeroFieldOptionUsageIfNoFieldsAssigned() {
    CustomField selectableField = createSelectableField();
    String optId = selectableField.getSelectField().getOptions().getValues().getFirst().getId();
    var stats = customFieldsClient.getCustomFieldOptionStats(selectableField.getId(), optId);

    assertThat(stats.getOptionId(), equalTo(optId));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(0));
  }

  @Test
  void shouldReturnFieldOptionUsageIfFieldAssigned() {
    CustomField selectableField = createSelectableField();
    String optId = selectableField.getSelectField().getOptions().getValues().getFirst().getId();
    assignValue(testUser, selectableField.getRefId(), List.of(optId));

    var stats = customFieldsClient.getCustomFieldOptionStats(selectableField.getId(), optId);

    assertThat(stats.getOptionId(), equalTo(optId));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(1));
  }

  @Test
  void shouldReturnFieldOptionUsageIfFieldAssignedWithSeveralValues() {
    CustomField selectableField = createSelectableField();
    String optId0 = selectableField.getSelectField().getOptions().getValues().get(0).getId();
    String optId1 = selectableField.getSelectField().getOptions().getValues().get(1).getId();
    assignValue(testUser, selectableField.getRefId(), optId0, optId1);

    var stats = customFieldsClient.getCustomFieldOptionStats(selectableField.getId(), optId0);

    assertThat(stats.getOptionId(), equalTo(optId0));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(1));

    stats = customFieldsClient.getCustomFieldOptionStats(selectableField.getId(), optId1);

    assertThat(stats.getOptionId(), equalTo(optId1));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(1));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  void shouldFailIfFieldOptionDoesntExist() {
    CustomField selectableField = createSelectableField();
    String fakeFieldOptId = "opt_10";
    customFieldsClient.attemptGetCustomFieldOptionStats(selectableField.getId(), fakeFieldOptId)
        .statusCode(SC_UNPROCESSABLE_ENTITY);
  }
}
