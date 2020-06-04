package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;

import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import io.restassured.http.Header;
import io.vertx.core.json.Json;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldCollection;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.SelectFieldOption;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.TestBase;
import org.folio.test.util.TokenTestUtil;

@RunWith(VertxUnitRunner.class)
public class CustomFieldRemovalTest extends TestBase {

  private static final String FAKE_FIELD_ID = "11111111-1111-1111-a111-111111111111";

  private static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  private static final Header USER8 = new Header(XOkapiHeaders.USER_ID, USER_ID);
  private static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);

  private static final String USERS_PATH = "users";
  private static final String CUSTOM_FIELDS_PATH = "custom-fields";
  private static final String SHORT_TEXT_FIELD_JSON_PATH = "fields/shortTextField.json";
  private static final String SINGLE_CHECKBOX_FIELD_JSON_PATH = "fields/singleCheckbox.json";
  private static final String MULTI_SELECT_FIELD_JSON_PATH = "fields/multiSelectField.json";

  private User user8;

  @Before
  public void setUp() throws IOException, URISyntaxException {
    user8 = createUser("users/user8.json");
  }

  @After
  public void tearDown() {
    CustomFieldsDBTestUtil.deleteAllCustomFields(vertx);

    deleteWithNoContent(USERS_PATH + "/" + user8.getId());
  }

  @Test
  public void shouldRemoveFieldIfNotAssignedToUser() throws IOException, URISyntaxException {
    CustomField textField = createField(SHORT_TEXT_FIELD_JSON_PATH);
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));
  }

  @Test
  public void shouldRemoveFieldAndItsValueIfAssignedToUser() throws IOException, URISyntaxException {
    // assign a value
    CustomField textField = createField(SHORT_TEXT_FIELD_JSON_PATH);
    assignValue(user8, textField.getRefId(), "someValue");

    // delete the field
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());
  }

  @Test
  public void shouldRemoveFieldAndAllValuesIfAssignedToSeveralUsers() throws IOException, URISyntaxException {
    CustomField textField = createField(SHORT_TEXT_FIELD_JSON_PATH);
    User user9 = createUser("users/user9.json");

    // assign a value to one user
    assignValue(user8, textField.getRefId(), "someValue1");

    // assign a value to another user
    assignValue(user9, textField.getRefId(), "someValue2");

    // delete the field
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user one doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), textField.getRefId());

    //validate user two doesn't have the field value
    validateFieldAbsent(getUser(user9.getId()), textField.getRefId());
  }

  @Test
  public void shouldRemoveTheSpecificFieldAndItsValueButNotTheOtherField() throws IOException, URISyntaxException {
    // assign values
    CustomField textField = createField(SHORT_TEXT_FIELD_JSON_PATH);
    CustomField checkbox = createField(SINGLE_CHECKBOX_FIELD_JSON_PATH);
    assignValue(user8, textField.getRefId(), "someValue1");
    assignValue(user8, checkbox.getRefId(), Boolean.FALSE);

    // delete the field
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

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
    deleteWithStatus(CUSTOM_FIELDS_PATH + "/" + FAKE_FIELD_ID, SC_NOT_FOUND);
  }

  @Test
  public void shouldRemoveOneValueFromOneAssignedWhenOneOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), "opt_2");

    // update the field. Remove "opt_2" option and no defaults
    multiSelectField.getSelectField().getOptions().getValues().remove(2);
    updateField(multiSelectField);

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), multiSelectField.getRefId());
  }

  @Test
  public void shouldRemoveOneValueFromTwoAssignedWhenOneOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

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
  public void shouldRemoveOneValueFromThreeAssignedWhenOneOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1", "opt_0"));

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
  public void shouldRemoveFieldAndAllValuesFromTwoAssignedWhenTwoOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

    // update the field. Remove "opt_1" and "opt_2" options and no defaults
    List<SelectFieldOption> values = multiSelectField.getSelectField().getOptions().getValues();
    values.remove(2);
    values.remove(1);
    updateField(multiSelectField);

    //validate user doesn't have the field value
    validateFieldAbsent(getUser(USER_ID), multiSelectField.getRefId());
  }

  @Test
  public void shouldRemoveOneValueAndSetOneDefaultFromOneAssignedWhenOneOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), "opt_2");

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
  public void shouldRemoveOneValueAndSetTwoDefaultFromOneAssignedWhenOneOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), "opt_2");

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
  public void shouldRemoveTwoValueAndSetTwoDefaultFromTwoAssignedWhenTwoOptionDeleted() throws IOException, URISyntaxException {
    // assign a value
    CustomField multiSelectField = createField(MULTI_SELECT_FIELD_JSON_PATH);
    assignValue(user8, multiSelectField.getRefId(), Arrays.asList("opt_2", "opt_1"));

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

  private CustomField createField(String pathToJson) throws IOException, URISyntaxException {
    return postWithStatus(CUSTOM_FIELDS_PATH, readFile(pathToJson), SC_CREATED, USER8, FAKE_TOKEN).as(CustomField.class);
  }

  private void updateField(CustomField field) {
    putWithStatus(CUSTOM_FIELDS_PATH + "/" + field.getId(), Json.encode(field), SC_NO_CONTENT, FAKE_TOKEN);
  }

  private User createUser(String pathToJson) throws IOException, URISyntaxException {
    String body = readFile(pathToJson);

    User user = postWithStatus(USERS_PATH, body, SC_CREATED, USER8).as(User.class);

    mockGetWithBody(new EqualToPattern("/" + USERS_PATH + "/" + user.getId()), body);

    return user;
  }

  private void assignValue(User user, String fieldRefId, Object value) {
    CustomFields fields = user.getCustomFields();
    if (fields == null) {
      fields = new CustomFields();
    }

    fields.setAdditionalProperty(fieldRefId, value);

    user.setCustomFields(fields);

    putWithNoContent(USERS_PATH + "/" + user.getId(), toJson(user), USER8);
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

  private User getUser(String userId) {
    return getWithOk("/" + USERS_PATH + "/" + userId).as(User.class);
  }
}
