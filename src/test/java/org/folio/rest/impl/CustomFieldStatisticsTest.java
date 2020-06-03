package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

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
import org.folio.rest.jaxrs.model.CustomFieldStatistic;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.TestBase;

@RunWith(VertxUnitRunner.class)
public class CustomFieldStatisticsTest extends TestBase {

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
    String user8Body = readFile("users/user8.json");
    user8 = createUser("users/user8.json");
    mockGetWithBody(new EqualToPattern("/" + USERS_PATH + "/" + user8.getId()), user8Body);
    textField = createField("fields/shortTextField.json");
  }

  @After
  public void tearDown() {
    CustomFieldsDBTestUtil.deleteAllCustomFields(vertx);
    deleteWithNoContent(USERS_PATH + "/" + user8.getId());
  }

  @Test
  public void shouldReturnZeroUsageIfNoFieldsAssigned() {
    CustomFieldStatistic stat = getWithOk(CUSTOM_FIELDS_PATH + "/" + textField.getId() + "/stats")
        .as(CustomFieldStatistic.class);

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(0));
  }

  @Test
  public void shouldReturnUsageCountIfFieldAssigned() {
    user8.withCustomFields(new CustomFields().withAdditionalProperty(textField.getRefId(), "someValue"));
    putWithNoContent(USERS_PATH + "/" + user8.getId(), toJson(user8), USER8);

    CustomFieldStatistic stat = getWithOk(CUSTOM_FIELDS_PATH + "/" + textField.getId() + "/stats")
      .as(CustomFieldStatistic.class);

    assertThat(stat.getFieldId(), equalTo(textField.getId()));
    assertThat(stat.getEntityType(), equalTo(textField.getEntityType()));
    assertThat(stat.getCount(), equalTo(1));
  }

  @Test
  public void shouldFailIfFieldDoesntExist() {
    getWithStatus(CUSTOM_FIELDS_PATH + "/" + FAKE_FIELD_ID + "/stats", SC_NOT_FOUND);
  }

  private User createUser(String pathToJson) throws IOException, URISyntaxException {
    String body = readFile(pathToJson);

    User user = postWithStatus(USERS_PATH, body, SC_CREATED, USER8).as(User.class);

    mockGetWithBody(new EqualToPattern("/" + USERS_PATH + "/" + user.getId()), body);

    return user;
  }

  private CustomField createField(String pathToJson) throws IOException, URISyntaxException {
    return postWithStatus(CUSTOM_FIELDS_PATH, readFile(pathToJson), SC_CREATED, USER8, FAKE_TOKEN)
      .as(CustomField.class);
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
