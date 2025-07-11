package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldCollection;
import org.folio.rest.jaxrs.model.SelectFieldOption;
import org.folio.support.User;
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
class CustomFieldRemovalIT extends CustomFieldTestBase {

  @Test
  void shouldRemoveFieldIfNotAssignedToUser() {
    var textField = createTextField();
    deleteField(textField.getId());

    CustomFieldCollection cfs = customFieldsClient.getAllCustomFields();

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));
  }

  @Test
  void shouldRemoveFieldAndItsValueIfAssignedToUser() {
    // assign a value
    var textField = createTextField();
    assignValue(testUser, textField.getRefId(), "someValue");

    // delete the field
    deleteField(textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = customFieldsClient.getAllCustomFields();

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());
  }

  @Test
  void shouldRemoveFieldAndAllValuesIfAssignedToSeveralUsers() {
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
    CustomFieldCollection cfs = customFieldsClient.getAllCustomFields();

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user one doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());

    //validate user two doesn't have the field value
    validateFieldAbsent(getUser(user9.getId()), textField.getRefId());
  }

  @Test
  void shouldRemoveTheSpecificFieldAndItsValueButNotTheOtherField() {
    // assign values
    CustomField textField = createTextField();
    CustomField checkbox = createCheckboxField();
    assignValue(testUser, textField.getRefId(), "someValue1");
    assignValue(testUser, checkbox.getRefId(), Boolean.FALSE);

    // delete the field
    deleteField(textField.getId());

    CustomFieldCollection cfs = customFieldsClient.getAllCustomFields();

    //validate there is one field left
    assertThat(cfs.getCustomFields(), hasSize(1));
    assertThat(cfs.getCustomFields().getFirst().getName(), equalTo(checkbox.getName()));
    assertThat(cfs.getTotalRecords(), equalTo(1));

    //validate one field is still assigned to user
    User user = getUser(USER_ID);
    assertThat(user.getCustomFields().size(), equalTo(1));
    assertThat(user.getCustomFields(), hasEntry(checkbox.getRefId(), Boolean.FALSE));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  void shouldFailIfFieldDoesntExist() {
    String fakeFieldId = "11111111-1111-1111-a111-111111111111";
    customFieldsClient.attemptToDeleteCustomField(fakeFieldId)
      .statusCode(SC_NOT_FOUND);
  }

  @Test
  void shouldRemoveOneValueFromOneAssignedWhenOneOptionDeleted() {
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
  void shouldRemoveOneValueFromTwoAssignedWhenOneOptionDeleted() {
    // assign a value
    var multiSelectField = createSelectableField();
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
  void shouldRemoveOneValueFromThreeAssignedWhenOneOptionDeleted() {
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
  void shouldRemoveFieldAndAllValuesFromTwoAssignedWhenTwoOptionDeleted() {
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
  void shouldRemoveOneValueAndSetOneDefaultFromOneAssignedWhenOneOptionDeleted() {
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
  void shouldRemoveOneValueAndSetTwoDefaultFromOneAssignedWhenOneOptionDeleted() {
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
  void shouldRemoveTwoValueAndSetTwoDefaultFromTwoAssignedWhenTwoOptionDeleted() {
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
    assertThat(user.getCustomFields(), not(hasKey(fieldRefId)));
  }

  private void validateValueAbsent(User user, String fieldRefId, String value) {
    Object assignedObject = user.getCustomFields().get(fieldRefId);
    if (assignedObject instanceof List) {
      @SuppressWarnings("unchecked")
      List<String> values = (List<String>) assignedObject;
      assertThat(values, not(hasItem(equalTo(value))));
    } else {
      assertThat(assignedObject, not(equalTo(value)));
    }
  }

  private void validateValueAssigned(User user, String fieldRefId, String value) {
    Object assignedObject = user.getCustomFields().get(fieldRefId);
    if (assignedObject instanceof List) {
      @SuppressWarnings("unchecked")
      List<String> values = (List<String>) assignedObject;
      assertThat(values, hasItem(equalTo(value)));
    } else {
      assertThat(assignedObject, equalTo(value));
    }
  }
}
