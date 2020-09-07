package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.List;

import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldCollection;
import org.folio.rest.jaxrs.model.SelectFieldOption;
import org.folio.rest.jaxrs.model.User;

@RunWith(VertxUnitRunner.class)
public class CustomFieldRemovalTest extends CustomFieldTestBase {

  @Test
  public void shouldRemoveFieldIfNotAssignedToUser() {
    CustomField textField = createTextField();
    deleteField(textField.getId());

    CustomFieldCollection cfs = getWithOk(cfEndpoint()).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));
  }

  @Test
  public void shouldRemoveFieldAndItsValueIfAssignedToUser() {
    // assign a value
    CustomField textField = createTextField();
    assignValue(testUser, textField.getRefId(), "someValue");

    // delete the field
    deleteField(textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = getWithOk(cfEndpoint()).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());
  }

  @Test
  public void shouldRemoveFieldAndAllValuesIfAssignedToSeveralUsers() {
    CustomField textField = createTextField();

    deleteUserIgnore("99999999-9999-4999-9999-999999999999");
    User user9 = createUser("users/user9.json");

    // assign a value to one user
    assignValue(testUser, textField.getRefId(), "someValue1");

    // assign a value to another user
    assignValue(user9, textField.getRefId(), "someValue2");

    // delete the field
    deleteField(textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = getWithOk(cfEndpoint()).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user one doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());

    //validate user two doesn't have the field value
    validateFieldAbsent(getUser(user9.getId()), textField.getRefId());
  }

  @Test
  public void shouldRemoveTheSpecificFieldAndItsValueButNotTheOtherField() {
    // assign values
    CustomField textField = createTextField();
    CustomField checkbox = createCheckboxField();
    assignValue(testUser, textField.getRefId(), "someValue1");
    assignValue(testUser, checkbox.getRefId(), Boolean.FALSE);

    // delete the field
    deleteField(textField.getId());

    CustomFieldCollection cfs = getWithOk(cfEndpoint()).as(CustomFieldCollection.class);

    //validate there is one field left
    assertThat(cfs.getCustomFields(), hasSize(1));
    assertThat(cfs.getCustomFields().get(0).getName(), equalTo(checkbox.getName()));
    assertThat(cfs.getTotalRecords(), equalTo(1));

    //validate one field is still assigned to user
    User user = getUser(USER_ID);
    assertThat(user.getCustomFields().getAdditionalProperties().size(), equalTo(1));
    assertThat(user.getCustomFields().getAdditionalProperties(), hasEntry(checkbox.getRefId(), Boolean.FALSE));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  public void shouldFailIfFieldDoesntExist() {
    String fakeFieldId = "11111111-1111-1111-a111-111111111111";
    deleteWithStatus(cfByIdEndpoint(fakeFieldId), SC_NOT_FOUND);
  }

  @Test
  public void shouldRemoveOneValueFromOneAssignedWhenOneOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), "opt_2");

    // update the field. Remove "opt_2" option and no defaults
    multiSelectField.getSelectField().getOptions().getValues().remove(2);
    updateField(multiSelectField);

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), multiSelectField.getRefId());
  }

  @Test
  public void shouldRemoveOneValueFromTwoAssignedWhenOneOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

    // update the field. Remove "opt_2" option and no defaults
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    SelectFieldOption optionToDelete = values.get(2);
    values.remove(optionToDelete);
    updateField(multiSelectField);

    //validate user doesn't have the option value
    User user = getUser(USER_ID);
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), "opt_1");
  }

  @Test
  public void shouldRemoveOneValueFromThreeAssignedWhenOneOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1", "opt_0"));

    // update the field. Remove "opt_2" option and no defaults
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    SelectFieldOption optionToDelete = values.get(2);
    values.remove(optionToDelete);
    updateField(multiSelectField);

    //validate user doesn't have the option value
    User user = getUser(USER_ID);
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), "opt_0");
    validateValueAssigned(user, multiSelectField.getRefId(), "opt_1");
  }

  @Test
  public void shouldRemoveFieldAndAllValuesFromTwoAssignedWhenTwoOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

    // update the field. Remove "opt_1" and "opt_2" options and no defaults
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    values.remove(2);
    values.remove(1);
    updateField(multiSelectField);

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), multiSelectField.getRefId());
  }

  @Test
  public void shouldRemoveOneValueAndSetOneDefaultFromOneAssignedWhenOneOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), "opt_2");

    // update the field. Remove "opt_2" option and set "opt_0" as default
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    SelectFieldOption optionToDefault = values.get(0);
    SelectFieldOption optionToDelete = values.get(2);
    values.remove(optionToDelete);
    optionToDefault.setDefault(true);
    updateField(multiSelectField);

    //validate user doesn't have the option value but has default value
    User user = getUser(USER_ID);
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), optionToDefault.getId());
  }

  @Test
  public void shouldRemoveOneValueAndSetTwoDefaultFromOneAssignedWhenOneOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), "opt_2");

    // update the field. Remove "opt_2" option and set "opt_0" as default
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    SelectFieldOption optionToDefault1 = values.get(0);
    SelectFieldOption optionToDefault2 = values.get(1);
    SelectFieldOption optionToDelete = values.get(2);
    values.remove(optionToDelete);
    optionToDefault1.setDefault(true);
    optionToDefault2.setDefault(true);
    updateField(multiSelectField);

    //validate user doesn't have the option value but has two default values
    User user = getUser(USER_ID);
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), optionToDefault1.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), optionToDefault2.getId());
  }

  @Test
  public void shouldRemoveTwoValueAndSetTwoDefaultFromTwoAssignedWhenTwoOptionDeleted() {
    // assign a value
    CustomField multiSelectField = createSelectableField();
    assignValue(testUser, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

    // update the field. Remove "opt_1" and "opt_2" options and set "opt_0" as default
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    SelectFieldOption optionToDefault = values.get(0);
    SelectFieldOption optionToDelete1 = values.get(1);
    SelectFieldOption optionToDelete2 = values.get(2);
    values.remove(optionToDelete1);
    values.remove(optionToDelete2);
    optionToDefault.setDefault(true);
    updateField(multiSelectField);

    //validate user doesn't have the option values but has default value
    User user = getUser(USER_ID);
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete1.getId());
    validateValueAbsent(user, multiSelectField.getRefId(), optionToDelete2.getId());
    validateValueAssigned(user, multiSelectField.getRefId(), optionToDefault.getId());
  }

  private void validateFieldAbsent(User user, String fieldRefId) {
    assertThat(user.getCustomFields().getAdditionalProperties(), not(hasKey(fieldRefId)));
  }

  private void validateValueAbsent(User user, String fieldRefId, String value) {
    Object assignedObject = user.getCustomFields().getAdditionalProperties().get(fieldRefId);
    if (assignedObject instanceof List) {
      @SuppressWarnings("unchecked")
      List<String> values = (List<String>) assignedObject;
      assertThat(values, not(hasItem(equalTo(value))));
    } else {
      assertThat(assignedObject, not(equalTo(value)));
    }
  }

  private void validateValueAssigned(User user, String fieldRefId, String value) {
    Object assignedObject = user.getCustomFields().getAdditionalProperties().get(fieldRefId);
    if (assignedObject instanceof List) {
      @SuppressWarnings("unchecked")
      List<String> values = (List<String>) assignedObject;
      assertThat(values, hasItem(equalTo(value)));
    } else {
      assertThat(assignedObject, equalTo(value));
    }
  }
}
