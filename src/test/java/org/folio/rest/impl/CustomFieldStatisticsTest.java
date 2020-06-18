package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldOptionStatistic;
import org.folio.rest.jaxrs.model.CustomFieldStatistic;

@RunWith(VertxUnitRunner.class)
public class CustomFieldStatisticsTest extends CustomFieldTestBase {

  @Test
  public void shouldReturnZeroFieldUsageIfNoValuesAssigned() {
    CustomField textField = createTextField();
    CustomFieldStatistic stat = getWithOk(cfByIdStatsEndpoint(textField.getId())).as(CustomFieldStatistic.class);

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(0));
  }

  @Test
  public void shouldReturnFieldUsageCountIfValuesAssigned() {
    CustomField textField = createTextField();
    assignValue(testUser, textField.getRefId(), "someValue");

    CustomFieldStatistic stat = getWithOk(cfByIdStatsEndpoint(textField.getId())).as(CustomFieldStatistic.class);

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(1));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  public void shouldFailIfFieldDoesntExist() {
    String fakeFieldId = "11111111-1111-1111-a111-111111111111";
    getWithStatus(cfByIdStatsEndpoint(fakeFieldId), SC_NOT_FOUND);
  }

  @Test
  public void shouldReturnZeroFieldOptionUsageIfNoFieldsAssigned() {
    CustomField selectableField = createSelectableField();
    String optId = selectableField.getSelectField().getOptions().getValues().get(0).getId();
    String resourcePath = cfByIdOptIdStatsEndpoint(selectableField.getId(), optId);
    CustomFieldOptionStatistic stats = getWithOk(resourcePath).as(CustomFieldOptionStatistic.class);

    assertThat(stats.getOptionId(), equalTo(optId));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(0));
  }

  @Test
  public void shouldReturnFieldOptionUsageIfNoFieldsAssigned() {
    CustomField selectableField = createSelectableField();
    String optId = selectableField.getSelectField().getOptions().getValues().get(0).getId();
    assignValue(testUser, selectableField.getRefId(), optId);

    String resourcePath = cfByIdOptIdStatsEndpoint(selectableField.getId(), optId);
    CustomFieldOptionStatistic stats = getWithOk(resourcePath).as(CustomFieldOptionStatistic.class);

    assertThat(stats.getOptionId(), equalTo(optId));
    assertThat(stats.getCustomFieldId(), equalTo(selectableField.getId()));
    assertThat(stats.getEntityType(), equalTo(selectableField.getEntityType()));
    assertThat(stats.getCount(), equalTo(1));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  public void shouldFailIfFieldOptionDoesntExist() {
    CustomField selectableField = createSelectableField();
    String fakeFieldOptId = "opt_10";
    getWithStatus(cfByIdOptIdStatsEndpoint(selectableField.getId(), fakeFieldOptId), SC_UNPROCESSABLE_ENTITY);
  }
}
