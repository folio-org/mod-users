package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.moduserstest.AbstractRestTest.KAFKA_ENV_VALUE;
import static org.folio.moduserstest.AbstractRestTest.KAFKA_IMAGE_NAME;
import static org.folio.moduserstest.AbstractRestTest.updateKafkaConfigField;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import io.restassured.http.Header;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.junit.runner.RunWith;

import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.DBTestUtil;
import org.folio.test.util.TestBase;
import org.folio.test.util.TokenTestUtil;
import org.testcontainers.kafka.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

@RunWith(VertxUnitRunner.class)
public class DepartmentsAPITest extends TestBase {

  private static final String DEPARTMENTS_ENDPOINT = "departments";
  private static final String USERS_ENDPOINT = "users";
  private static final String USER_JSON_PATH = "users/user8.json";

  private static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  private static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);

  protected User testUser;
  private static final KafkaContainer kafkaContainer = new KafkaContainer(
    DockerImageName.parse(KAFKA_IMAGE_NAME));

  private static final ExternalResource resource = new ExternalResource() {
    @Override
    protected void before() {
      kafkaContainer.start();
      updateKafkaConfigField("envId", KAFKA_ENV_VALUE);
      updateKafkaConfigField("kafkaHost", kafkaContainer.getHost());
      updateKafkaConfigField("kafkaPort", String.valueOf(kafkaContainer.getFirstMappedPort()));
    }

    @Override
    protected void after() {
      kafkaContainer.stop();
    }
  };

  @ClassRule
  public static final TestRule rules = RuleChain.outerRule(resource);

  @Before
  public void setUp() {
    testUser = createUser();
  }

  @After
  public void tearDown() {
    deleteWithNoContent(USERS_ENDPOINT + "/" + testUser.getId());
    DBTestUtil.deleteFromTable(vertx, "departments");
  }

  @Test
  public void shouldSaveValidDepartmentOnPost() {
    Department expected = createDepartment(null, "name", "code");
    Department actual = post(expected);

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getName(), equalTo(expected.getName()));
    assertThat(actual.getCode(), equalTo(expected.getCode()));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  public void shouldSaveSpecialCharacters() {
    String name = "<script>\n"
        + "document.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n"
        + "</script><b>foo</b>";
    String code =  "<script>\n"
        + "document.getElementById(\"demo\").innerHTML = \"Bye JavaScript!\";\n"
        + "</script>bar&pi;";
    Department actual = post(createDepartment(null, name, code));

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getName(), equalTo(name));
    assertThat(actual.getCode(), equalTo(code));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  public void shouldReturn422OnPostWithDuplicateId() {
    String id = UUID.randomUUID().toString();
    Department dep1 = createDepartment(id, "name1", "code1");
    Department dep2 = createDepartment(id, "name2", "code2");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this id already exists"));
  }

  @Test
  public void shouldReturn422OnPostWithDuplicateName() {
    Department dep1 = createDepartment(null, "name", "code1");
    Department dep2 = createDepartment(null, "name", "code2");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this name already exists"));
  }

  @Test
  public void shouldReturn422OnPostWithDuplicateCode() {
    Department dep1 = createDepartment(null, "name1", "code");
    Department dep2 = createDepartment(null, "name2", "code");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this code already exists"));
  }

  private void assertErrorAboutEmptyNameAndCode(Errors errors) {
    List<Parameter> parameters = errors.getErrors().stream()
        .map(error -> error.getParameters().get(0)).collect(Collectors.toList());
    assertThat(parameters, containsInAnyOrder(
        new Parameter().withKey("name").withValue("null"),
        new Parameter().withKey("code").withValue("null") ));
  }

  @Test
  public void shouldReturn422OnPostWithEmptyNameAndCode() {
    Department dep1 = createDepartment(null, null, null);
    assertErrorAboutEmptyNameAndCode(postWithError(dep1));
  }

  @Test
  public void shouldReturn400OnAssignNotExistedDepartmentToUser() {
    String notExistedId = UUID.randomUUID().toString();
    Department department = post(createDepartment(null, "name1", "code"));
    String errorMessage = assignDepartmentWithError(testUser, department.getId(), notExistedId);
    assertThat(errorMessage, containsString("Key (departments)=(" + notExistedId + ") is not present"));
  }

  @Test
  public void shouldUpdateDepartmentOnValidPut() {
    Department department = post(createDepartment(null, "name", "code"));
    department.setName("name new");
    department.setName("code new");
    Department actual = put(department);

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getName(), equalTo(department.getName()));
    assertThat(actual.getCode(), equalTo(department.getCode()));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  public void shouldReturn422OnPutWithDuplicateName() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.setName(dep1.getName());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this name already exists"));
  }

  @Test
  public void shouldReturn422OnPutWithDuplicateCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.setCode(dep1.getCode());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this code already exists"));
  }

  @Test
  public void shouldReturn422OnPutWithEmptyNameAndCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    assertErrorAboutEmptyNameAndCode(putWithError(dep1.withCode(null).withName(null)));
  }

  @Test
  public void shouldReturn404OnPutByRandomId() {
    Department department = createDepartment(UUID.randomUUID().toString(), "name1", "code1");
    String errorMessage =
      putWithStatus(itemEndpoint(department.getId()), toJson(department), SC_NOT_FOUND, FAKE_TOKEN).asString();
    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  public void shouldReturn400OnPutByInvalidId() {
    Department department = createDepartment(UUID.randomUUID().toString(), "name1", "code1");
    String errorMessage =
      putWithStatus(itemEndpoint("invalid-id"), toJson(department), SC_BAD_REQUEST, FAKE_TOKEN).asString();
    assertThat(errorMessage, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  public void shouldReturnDepartmentCollectionOnGet() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep1.setUsageNumber(0);
    dep2.setUsageNumber(0);

    DepartmentCollection actual = getCollection(null);
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(2));
    assertThat(actual.getTotalRecords(), equalTo(2));
    assertThat(actual.getDepartments(), hasItems(dep1, dep2));
  }

  @Test
  public void shouldReturnEmptyDepartmentCollectionOnGet() {
    DepartmentCollection actual = getCollection(null);
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(0));
    assertThat(actual.getTotalRecords(), equalTo(0));
  }

  @Test
  public void shouldReturnDepartmentCollectionOnGetByQuery() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep1.setUsageNumber(0);
    dep2.setUsageNumber(0);

    DepartmentCollection actual = getCollection("code==\"*1*\"");
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(1));
    assertThat(actual.getTotalRecords(), equalTo(1));
    assertThat(actual.getDepartments(), hasItem(dep1));
  }

  @Test
  public void shouldReturnDepartmentCollectionWithUsageNumberOnGet() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    expected.setUsageNumber(1);
    assignDepartment(testUser, id);

    DepartmentCollection actual = getCollection(null);
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(1));
    assertThat(actual.getTotalRecords(), equalTo(1));
    assertThat(actual.getDepartments(), hasItem(expected));
  }

  @Test
  public void shouldReturnDepartmentOnGetById() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    expected.setUsageNumber(0);

    Department actual = getItem(id);
    assertThat(actual, equalTo(expected));
  }

  @Test
  public void shouldReturnDepartmentWithUsageNumberOnGetById() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    assignDepartment(testUser, id);

    expected.setUsageNumber(1);

    Department actual = getItem(id);
    assertThat(actual, equalTo(expected));
  }

  @Test
  public void shouldReturn404OnGetByRandomId() {
    String errorMessage = getItemWithError(UUID.randomUUID().toString(), SC_NOT_FOUND);
    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  public void shouldReturn400OnGetByInvalidId() {
    String errors = getItemWithError("invalid-id", SC_BAD_REQUEST);
    assertThat(errors, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  public void shouldReturn204OnDeleteWhenAssignedToUser() {
    String id = UUID.randomUUID().toString();
    post(createDepartment(id, "name1", "code1"));
    deleteWithNoContent(itemEndpoint(id));

    List<Department> departmentsInDB = DBTestUtil.getAll(Department.class, vertx, "departments");
    assertThat(departmentsInDB, empty());
  }

  @Test
  public void shouldReturn400OnDeleteWhenAssignedToUser() {
    String id = UUID.randomUUID().toString();
    post(createDepartment(id, "name1", "code1"));
    assignDepartment(testUser, id);

    String errorMessage = deleteItemWithError(id, SC_BAD_REQUEST);
    assertThat(errorMessage, containsString("Cannot delete departments"));

    List<Department> departmentsInDB = DBTestUtil.getAll(Department.class, vertx, "departments");
    assertThat(departmentsInDB, hasSize(1));
  }

  @Test
  public void shouldReturn404OnDeleteByRandomId() {
    String errorMessage = deleteItemWithError(UUID.randomUUID().toString(), SC_NOT_FOUND);
    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  public void shouldReturn400OnDeleteByInvalidId() {
    String errors = deleteItemWithError("invalid-id", SC_BAD_REQUEST);
    assertThat(errors, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  public void handleUniqueConstraintViolationWithNullResult() {
    DepartmentsAPI.handleUniqueConstraintViolation(null, null, null, result -> {
      assertThat(result, nullValue());
    });
  }

  private DepartmentCollection getCollection(String query) {
    return getWithOk(collectionEndpoint() + (query == null ? "" : "?query=" + query)).as(DepartmentCollection.class);
  }

  private Department getItem(String id) {
    return getWithOk(itemEndpoint(id)).as(Department.class);
  }

  private String getItemWithError(String id, int code) {
    return getWithStatus(itemEndpoint(id), code).asString();
  }

  private Department post(Department department) {
    return postWithStatus(collectionEndpoint(), toJson(department), SC_CREATED, FAKE_TOKEN).as(Department.class);
  }

  private Errors postWithError(Department department) {
    return postWithStatus(collectionEndpoint(), toJson(department), SC_UNPROCESSABLE_ENTITY, FAKE_TOKEN).as(Errors.class);
  }

  private Department put(Department department) {
    putWithNoContent(itemEndpoint(department.getId()), toJson(department), FAKE_TOKEN);
    return getItem(department.getId());
  }

  private Errors putWithError(Department department) {
    return putWithStatus(itemEndpoint(department.getId()), toJson(department), SC_UNPROCESSABLE_ENTITY, FAKE_TOKEN)
      .as(Errors.class);
  }

  private String deleteItemWithError(String id, int code) {
    return deleteWithStatus(itemEndpoint(id), code).asString();
  }

  private void assignDepartment(User user, String... departmentIds) {
    user.setDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    putWithNoContent(USERS_ENDPOINT + "/" + user.getId(), toJson(user), FAKE_TOKEN);
  }

  private String assignDepartmentWithError(User user, String... departmentIds) {
    user.setDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    return putWithStatus(USERS_ENDPOINT + "/" + user.getId(), toJson(user), SC_BAD_REQUEST, FAKE_TOKEN).asString();
  }

  private Department createDepartment(String id, String name, String code) {
    return new Department()
      .withId(id)
      .withName(name)
      .withCode(code);
  }

  private String collectionEndpoint() {
    return DEPARTMENTS_ENDPOINT;
  }

  private String itemEndpoint(String departmentId) {
    return String.join("/", collectionEndpoint(), departmentId);
  }

  private User createUser() {
    String body;
    try {
      body = readFile(USER_JSON_PATH);
    } catch (IOException | URISyntaxException e) {
      Assert.fail(e.getMessage());
      body = null;
    }

    User user = postWithStatus(USERS_ENDPOINT, body, SC_CREATED, FAKE_TOKEN).as(User.class);

    mockGetWithBody(new EqualToPattern("/" + USERS_ENDPOINT + "/" + user.getId()), body);

    return user;
  }
}
