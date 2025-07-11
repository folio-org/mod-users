package org.folio.rest.impl;

import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.Arrays;
import java.util.LinkedHashMap;

import io.restassured.http.Header;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;

import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.support.User;
import org.folio.support.http.CustomFieldsClient;
import org.folio.support.http.UsersClient;
import org.folio.test.util.TokenTestUtil;

public class CustomFieldTestBase extends AbstractRestTestNoData {

  protected static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  protected static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);
  protected static final Header FAKE_USER_ID = new Header(OKAPI_USERID_HEADER, USER_ID);

  private static final String USER_JSON_PATH = "users/user8.json";
  private static final String SHORT_TEXT_FIELD_JSON_PATH = "fields/shortTextField.json";
  private static final String SINGLE_CHECKBOX_FIELD_JSON_PATH = "fields/singleCheckbox.json";
  private static final String MULTI_SELECT_FIELD_JSON_PATH = "fields/multiSelectField.json";

  protected static UsersClient usersClient;
  protected static CustomFieldsClient customFieldsClient;

  protected User testUser;

  @BeforeAll
  static void beforeAll() {
    customFieldsClient = new CustomFieldsClient(okapiUrl, okapiHeaders);
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  void setUp() {
    testUser = createUser(USER_JSON_PATH);
  }

  @AfterEach
  void tearDown() {
    usersClient.deleteAllUsers();
    customFieldsClient.deleteAllCustomFields();
  }

  protected CustomField createTextField() {
    return createField(SHORT_TEXT_FIELD_JSON_PATH);
  }

  protected CustomField createCheckboxField() {
    return createField(SINGLE_CHECKBOX_FIELD_JSON_PATH);
  }

  protected CustomField createSelectableField() {
    return createField(MULTI_SELECT_FIELD_JSON_PATH);
  }

  protected void updateField(CustomField field) {
    customFieldsClient.updateCustomField(field, FAKE_USER_ID, FAKE_TOKEN);
  }

  protected void deleteField(String fieldId) {
    customFieldsClient.deleteCustomField(fieldId);
  }

  protected User createUser(String pathToJson) {
    var user = readObjectFromFile(pathToJson, User.class);
    return usersClient.createUser(user);
  }

  @SuppressWarnings("SameParameterValue")
  protected void deleteUserIgnore(String userId) {
    usersClient.attemptToDeleteUser(userId);
  }

  protected User getUser(String userId) {
    return usersClient.getUser(userId);
  }

  protected void assignValue(User user, String fieldRefId, Object... values) {
    var fields = user.getCustomFields();
    if (fields == null) {
      fields = new LinkedHashMap<>();
    }

    fields.put(fieldRefId, values.length == 1 ? values[0] : Arrays.asList(values));
    usersClient.updateUser(user);
  }

  private CustomField createField(String pathToJson) {
    var customField = readObjectFromFile(pathToJson, CustomField.class);
    return customFieldsClient.createCustomField(customField, FAKE_USER_ID, FAKE_TOKEN);
  }
}
