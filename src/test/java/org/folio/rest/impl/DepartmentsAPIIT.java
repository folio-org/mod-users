package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import io.restassured.http.Header;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.support.User;
import org.folio.support.http.DepartmentsClient;
import org.folio.support.http.UsersClient;
import org.folio.support.tags.IntegrationTest;
import org.folio.test.util.TokenTestUtil;

@IntegrationTest
class DepartmentsAPIIT extends AbstractRestTestNoData {

  private static final String USER_JSON_PATH = "users/user8.json";
  private static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  private static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);
  private static final Header FAKE_USER_ID = new Header(OKAPI_USERID_HEADER, USER_ID);

  private static UsersClient usersClient;
  private static DepartmentsClient departmentsClient;

  protected User testUser;

  @BeforeAll
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    departmentsClient = new DepartmentsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  void setUp() {
    testUser = createUser();
  }

  @AfterEach
  void tearDown() {
    usersClient.deleteAllUsers();
    departmentsClient.deleteAllDepartments();
  }

  @Test
  void shouldSaveValidDepartmentOnPost() {
    Department expected = createDepartment(null, "name", "code");
    Department actual = post(expected);

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getName(), equalTo(expected.getName()));
    assertThat(actual.getCode(), equalTo(expected.getCode()));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  void shouldSaveSpecialCharacters() {
    String name = """
      <script>
        document.getElementById("demo").innerHTML = "Hello JavaScript!";
      </script><b>foo</b>""";
    String code = """
      <script>
        document.getElementById("demo").innerHTML = "Bye JavaScript!";
      </script>bar&pi;""";
    Department actual = post(createDepartment(null, name, code));

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getName(), equalTo(name));
    assertThat(actual.getCode(), equalTo(code));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  void shouldReturn422OnPostWithDuplicateId() {
    String id = UUID.randomUUID().toString();
    Department dep1 = createDepartment(id, "name1", "code1");
    Department dep2 = createDepartment(id, "name2", "code2");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().getFirst().getMessage(), containsString("Department with this id already exists"));
  }

  @Test
  void shouldReturn422OnPostWithDuplicateName() {
    Department dep1 = createDepartment(null, "name", "code1");
    Department dep2 = createDepartment(null, "name", "code2");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().getFirst().getMessage(), containsString("Department with this name already exists"));
  }

  @Test
  void shouldReturn422OnPostWithDuplicateCode() {
    Department dep1 = createDepartment(null, "name1", "code");
    Department dep2 = createDepartment(null, "name2", "code");
    post(dep1);
    Errors errors = postWithError(dep2);
    assertThat(errors.getErrors().getFirst().getMessage(), containsString("Department with this code already exists"));
  }

  private void assertErrorAboutEmptyNameAndCode(Errors errors) {
    List<Parameter> parameters = errors.getErrors().stream()
      .map(error -> error.getParameters().getFirst()).toList();
    assertThat(parameters, containsInAnyOrder(
      new Parameter().withKey("name").withValue("null"),
      new Parameter().withKey("code").withValue("null")));
  }

  @Test
  void shouldReturn422OnPostWithEmptyNameAndCode() {
    Department dep1 = createDepartment(null, null, null);
    assertErrorAboutEmptyNameAndCode(postWithError(dep1));
  }

  @Test
  void shouldReturn400OnAssignNotExistedDepartmentToUser() {
    String notExistedId = UUID.randomUUID().toString();
    Department department = post(createDepartment(null, "name1", "code"));
    String errorMessage = assignDepartmentWithError(testUser, department.getId(), notExistedId);
    assertThat(errorMessage, containsString("Key (departments)=(" + notExistedId + ") is not present"));
  }

  @Test
  void shouldUpdateDepartmentOnValidPut() {
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
  void shouldReturn422OnPutWithDuplicateName() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.setName(dep1.getName());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().getFirst().getMessage(), containsString("Department with this name already exists"));
  }

  @Test
  void shouldReturn422OnPutWithDuplicateCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.setCode(dep1.getCode());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().getFirst().getMessage(), containsString("Department with this code already exists"));
  }

  @Test
  void shouldReturn422OnPutWithEmptyNameAndCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    assertErrorAboutEmptyNameAndCode(putWithError(dep1.withCode(null).withName(null)));
  }

  @Test
  void shouldReturn404OnPutByRandomId() {
    Department department = createDepartment(UUID.randomUUID().toString(), "name1", "code1");
    var errorMessage = departmentsClient.attemptUpdateDepartment(department, FAKE_USER_ID, FAKE_TOKEN)
      .statusCode(SC_NOT_FOUND)
      .extract().asString();

    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  void shouldReturn400OnPutByInvalidId() {
    Department department = createDepartment(UUID.randomUUID().toString(), "name1", "code1");
    var errorMessage = departmentsClient.attemptUpdateDepartment(
        "invalid-id", department, FAKE_USER_ID, FAKE_TOKEN)
      .statusCode(SC_BAD_REQUEST)
      .extract().asString();
    assertThat(errorMessage, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  void shouldReturnDepartmentCollectionOnGet() {
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
  void shouldReturnEmptyDepartmentCollectionOnGet() {
    DepartmentCollection actual = getCollection(null);
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(0));
    assertThat(actual.getTotalRecords(), equalTo(0));
  }

  @Test
  void shouldReturnDepartmentCollectionOnGetByQuery() {
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
  void shouldReturnDepartmentCollectionWithUsageNumberOnGet() {
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
  void shouldReturnDepartmentOnGetById() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    expected.setUsageNumber(0);

    Department actual = getItem(id);
    assertThat(actual, equalTo(expected));
  }

  @Test
  void shouldReturnDepartmentWithUsageNumberOnGetById() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    assignDepartment(testUser, id);

    expected.setUsageNumber(1);

    Department actual = getItem(id);
    assertThat(actual, equalTo(expected));
  }

  @Test
  void shouldReturn404OnGetByRandomId() {
    var errorMessage = departmentsClient.attemptGetDepartment(UUID.randomUUID().toString())
      .statusCode(SC_NOT_FOUND)
      .extract().asString();
    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  void shouldReturn400OnGetByInvalidId() {
    String errors = getItemWithError("invalid-id", SC_BAD_REQUEST);
    assertThat(errors, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  void shouldReturn204OnDeleteWhenAssignedToUser() {
    String id = UUID.randomUUID().toString();
    post(createDepartment(id, "name1", "code1"));
    departmentsClient.deleteDepartment(id);

    var departmentsCollection = departmentsClient.getAllDepartments();
    assertThat(departmentsCollection.getTotalRecords(), is(0));
    assertThat(departmentsCollection.getDepartments(), empty());
  }

  @Test
  void shouldReturn400OnDeleteWhenAssignedToUser() {
    String id = UUID.randomUUID().toString();
    post(createDepartment(id, "name1", "code1"));
    assignDepartment(testUser, id);

    String errorMessage = deleteItemWithError(id, SC_BAD_REQUEST);
    assertThat(errorMessage, containsString("Cannot delete departments"));

    var departmentsCollection = departmentsClient.getAllDepartments();
    assertThat(departmentsCollection.getTotalRecords(), is(1));
    assertThat(departmentsCollection.getDepartments(), hasSize(1));
  }

  @Test
  void shouldReturn404OnDeleteByRandomId() {
    String errorMessage = deleteItemWithError(UUID.randomUUID().toString(), SC_NOT_FOUND);
    assertThat(errorMessage, equalTo("Not found"));
  }

  @Test
  void shouldReturn400OnDeleteByInvalidId() {
    String errors = deleteItemWithError("invalid-id", SC_BAD_REQUEST);
    assertThat(errors, containsString("'departmentId' parameter is incorrect"));
  }

  @Test
  void handleUniqueConstraintViolationWithNullResult() {
    DepartmentsAPI.handleUniqueConstraintViolation(null, null, null, result -> {
      assertThat(result, nullValue());
    });
  }

  private DepartmentCollection getCollection(String query) {
    return query == null
      ? departmentsClient.getAllDepartments()
      : departmentsClient.getDepartments(query);
  }

  private Department getItem(String id) {
    return departmentsClient.getDepartment(id);
  }

  @SuppressWarnings("SameParameterValue")
  private String getItemWithError(String id, int code) {
    return departmentsClient.attemptGetDepartment(id)
      .statusCode(code)
      .extract().asString();
  }

  private Department post(Department department) {
    return departmentsClient.createDepartment(department, FAKE_USER_ID, FAKE_TOKEN);
  }

  private Errors postWithError(Department department) {
    return departmentsClient.attemptCreateDepartment(department, FAKE_USER_ID, FAKE_TOKEN)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract()
      .as(Errors.class);
  }

  private Department put(Department department) {
    departmentsClient.updateDepartment(department, FAKE_USER_ID, FAKE_TOKEN);
    return getItem(department.getId());
  }

  private Errors putWithError(Department department) {
    return departmentsClient.attemptUpdateDepartment(department, FAKE_USER_ID, FAKE_TOKEN)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract()
      .as(Errors.class);
  }

  private String deleteItemWithError(String id, int code) {
    return departmentsClient.attemptDeleteDepartment(id).statusCode(code).extract().asString();
  }

  private void assignDepartment(User user, String... departmentIds) {
    var userWithDepartments = user.withDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    usersClient.updateUser(userWithDepartments);
  }

  private String assignDepartmentWithError(User user, String... departmentIds) {
    var userWithDepartments = user.withDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    return usersClient.attemptToUpdateUser(userWithDepartments)
      .statusCode(SC_BAD_REQUEST)
      .extract()
      .asString();
  }

  private Department createDepartment(String id, String name, String code) {
    return new Department()
      .withId(id)
      .withName(name)
      .withCode(code);
  }

  private User createUser() {
    var user = readObjectFromFile(USER_JSON_PATH, User.class);
    return usersClient.createUser(user);
  }
}
