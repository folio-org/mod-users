package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.UUID;

import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFieldCollection;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.TestBase;

@RunWith(VertxUnitRunner.class)
public class CustomFieldRemovalTest extends TestBase {

  private static final String FAKE_FIELD_ID = "11111111-1111-1111-a111-111111111111";

  private static final String USER_ID =  "88888888-8888-4888-8888-888888888888";
  private static final Header USER8 = new Header(XOkapiHeaders.USER_ID, USER_ID);
  private static final Header FAKE_TOKEN = new Header(XOkapiHeaders.TOKEN, makeFakeJWT("mockuser8", USER_ID, "diku"));

  private static final String USERS_PATH = "users";
  private static final String CUSTOM_FIELDS_PATH = "custom-fields";

  private User user8;
  private CustomField textField;

  @Before
  public void setUp() throws IOException, URISyntaxException {
    user8 = createUser("users/user8.json");

    textField = createField("fields/shortTextField.json");
  }

  @After
  public void tearDown() {
    CustomFieldsDBTestUtil.deleteAllCustomFields(vertx);

    deleteWithNoContent(USERS_PATH + "/" + user8.getId());
  }

  @Test
  public void shouldRemoveFieldIfNotAssignedToUser() {
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));
  }

  @Test
  public void shouldRemoveFieldAndItsValueIfAssignedToUser() {
    // assign a value
    assignValue(user8, textField.getRefId(), "someValue");

    // delete the field
    deleteWithNoContent(CUSTOM_FIELDS_PATH + "/" + textField.getId());

    //validate there is no field defined
    CustomFieldCollection cfs = getWithOk(CUSTOM_FIELDS_PATH).as(CustomFieldCollection.class);

    assertThat(cfs.getCustomFields(), empty());
    assertThat(cfs.getTotalRecords(), equalTo(0));

    //validate user doesn't have the field value
    validateFieldAbsent(user8.getId(), textField.getRefId());
  }

  @Test
  public void shouldRemoveFieldAndAllValuesIfAssignedToSeveralUsers() throws IOException, URISyntaxException {
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
    validateFieldAbsent(user8.getId(), textField.getRefId());

    //validate user two doesn't have the field value
    validateFieldAbsent(user9.getId(), textField.getRefId());
  }

  @Test
  public void shouldRemoveTheSpecificFieldAndItsValueButNotTheOtherField() throws IOException, URISyntaxException {
    CustomField checkbox = createField("fields/singleCheckbox.json");

    // assign values
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
    user8 = getWithOk("/" + USERS_PATH + "/" + user8.getId()).as(User.class);
    assertThat(user8.getCustomFields().getAdditionalProperties().size(), equalTo(1));
    assertThat(user8.getCustomFields().getAdditionalProperties(), hasEntry(checkbox.getRefId(), Boolean.FALSE));
  }

  @Test
  @SuppressWarnings("squid:S2699")
  public void shouldFailIfFieldDoesntExist() {
    deleteWithStatus(CUSTOM_FIELDS_PATH + "/" + FAKE_FIELD_ID, SC_NOT_FOUND);
  }

  private CustomField createField(String pathToJson) throws IOException, URISyntaxException {
    return postWithStatus(CUSTOM_FIELDS_PATH, readFile(pathToJson), SC_CREATED, USER8, FAKE_TOKEN)
      .as(CustomField.class);
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

  private void validateFieldAbsent(String userId, String fieldRefId) {
    User user = getWithOk("/" + USERS_PATH + "/" + userId).as(User.class);
    assertThat(user.getCustomFields().getAdditionalProperties(), not(hasKey(fieldRefId)));
  }

  private static String makeFakeJWT(String username, String id, String tenant) {
    JsonObject header = new JsonObject()
      .put("alg", "HS512");
    JsonObject payload = new JsonObject()
      .put("sub", username)
      .put("user_id", id)
      .put("tenant", tenant);
    return String.format("%s.%s.%s",
      Base64.getEncoder().encodeToString(header.encode()
        .getBytes(StandardCharsets.UTF_8)),
      Base64.getEncoder().encodeToString(payload.encode()
        .getBytes(StandardCharsets.UTF_8)),
      Base64.getEncoder().encodeToString((header.encode() + payload.encode())
        .getBytes(StandardCharsets.UTF_8)));
  }
  
}
