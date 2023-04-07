package org.folio.rest.impl;

import static io.restassured.RestAssured.given;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;

import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;

import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import io.restassured.http.Header;
import io.vertx.core.json.Json;
import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.TestBase;
import org.folio.test.util.TokenTestUtil;

public class CustomFieldTestBase extends TestBase {
  protected static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  protected static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);
  protected static final Header FAKE_USER_ID = new Header(OKAPI_USERID_HEADER, USER_ID);

  private static final String USERS_ENDPOINT = "users";
  private static final String CUSTOM_FIELDS_ENDPOINT = "custom-fields";

  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private static final String USER_JSON_PATH = "users/user8.json";
  private static final String SHORT_TEXT_FIELD_JSON_PATH = "fields/shortTextField.json";
  private static final String SINGLE_CHECKBOX_FIELD_JSON_PATH = "fields/singleCheckbox.json";
  private static final String MULTI_SELECT_FIELD_JSON_PATH = "fields/multiSelectField.json";

  protected User testUser;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeClass
  public static void setUpClass() {
    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);
  }

  @Before
  public void setUp() {
    testUser = createUser(USER_JSON_PATH);
  }

  @After
  public void tearDown() {
    CustomFieldsDBTestUtil.deleteAllCustomFields(vertx);
    deleteWithNoContent(USERS_ENDPOINT + "/" + testUser.getId());
  }

  @AfterClass
  public static void tearDownClass() {
    kafkaCluster.stop();
  }


  protected String cfEndpoint() {
    return CUSTOM_FIELDS_ENDPOINT;
  }

  protected String cfByIdEndpoint(String fieldId) {
    return String.join("/", cfEndpoint(), fieldId);
  }

  protected String cfByIdStatsEndpoint(String fieldId) {
    return String.join("/", cfByIdEndpoint(fieldId), "stats");
  }

  protected String cfByIdOptIdStatsEndpoint(String fieldId, String optId) {
    return String.join("/", cfByIdEndpoint(fieldId), "options", optId, "stats");
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
    putWithStatus(cfByIdEndpoint(field.getId()), Json.encode(field),
      SC_NO_CONTENT, FAKE_TOKEN, FAKE_USER_ID);
  }

  protected void deleteField(String fieldId) {
    deleteWithNoContent(cfByIdEndpoint(fieldId));
  }

  protected User createUser(String pathToJson) {
    String body = readExistedFile(pathToJson);

    User user = postWithStatus(USERS_ENDPOINT, body, SC_CREATED, FAKE_TOKEN).as(User.class);

    mockGetWithBody(new EqualToPattern("/" + USERS_ENDPOINT + "/" + user.getId()), body);

    return user;
  }

  protected void deleteUserIgnore(String userId) {
    given()
    .spec(getRequestSpecification())
    .when()
    .delete("/" + USERS_ENDPOINT + "/" + userId)
    .then()
    .log().ifValidationFails();
  }

  protected User getUser(String userId) {
    return getWithOk("/" + USERS_ENDPOINT + "/" + userId).as(User.class);
  }

  protected void assignValue(User user, String fieldRefId, Object... values) {
    CustomFields fields = user.getCustomFields();
    if (fields == null) {
      fields = new CustomFields();
    }

    fields.setAdditionalProperty(fieldRefId, values.length == 1 ? values[0] : Arrays.asList(values));

    user.setCustomFields(fields);

    putWithNoContent(USERS_ENDPOINT + "/" + user.getId(), toJson(user), FAKE_TOKEN);
  }

  private CustomField createField(String pathToJson) {
    return postWithStatus(cfEndpoint(), readExistedFile(pathToJson), SC_CREATED,
      FAKE_TOKEN, FAKE_USER_ID).as(CustomField.class);
  }

  private String readExistedFile(String pathToJson) {
    try {
      return readFile(pathToJson);
    } catch (IOException | URISyntaxException e) {
      Assert.fail(e.getMessage());
      return null;
    }
  }
}
