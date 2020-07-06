package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import static org.folio.test.util.TestUtil.mockGetWithBody;
import static org.folio.test.util.TestUtil.readFile;
import static org.folio.test.util.TestUtil.toJson;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import com.github.tomakehurst.wiremock.matching.EqualToPattern;
import io.restassured.http.Header;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentAttributes;
import org.folio.rest.jaxrs.model.DepartmentCollection;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.User;
import org.folio.test.util.DBTestUtil;
import org.folio.test.util.TestBase;
import org.folio.test.util.TokenTestUtil;

@RunWith(VertxUnitRunner.class)
public class DepartmentsAPITest extends TestBase {

  private static final String DEPARTMENTS_ENDPOINT = "departments";
  private static final String USERS_ENDPOINT = "users";

  private static final String USER_JSON_PATH = "users/user8.json";

  private static final String USER_ID = "88888888-8888-4888-8888-888888888888";
  private static final Header FAKE_TOKEN = TokenTestUtil.createTokenHeader("mockuser8", USER_ID);

  protected User testUser;

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
    assertThat(actual.getType(), equalTo(expected.getType()));
    assertThat(actual.getAttributes().getName(), equalTo(expected.getAttributes().getName()));
    assertThat(actual.getAttributes().getCode(), equalTo(expected.getAttributes().getCode()));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  public void shouldSaveAndCleanDepartmentOnPost() {
    String name = "name";
    String code = "code";
    Department expected = createDepartment(null, "<script>\n"
      + "document.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n"
      + "</script><b>" + name + "</b>", "<script>\n"
      + "document.getElementById(\"demo\").innerHTML = \"Bye JavaScript!\";\n"
      + "</script>" + code);
    Department actual = post(expected);

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getType(), equalTo(expected.getType()));
    assertThat(actual.getAttributes().getName(), equalTo(name));
    assertThat(actual.getAttributes().getCode(), equalTo(code));
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

  @Test
  public void shouldReturn422OnPostWithEmptyNameAndCode() {
    Department dep1 = createDepartment(null, null, null);
    Errors errors = postWithError(dep1);
    assertThat(errors.getErrors(), hasSize(2));
    for (Error error : errors.getErrors()) {
      assertThat(error.getMessage(), equalTo("may not be null"));
      assertThat(error.getParameters().get(0).getKey(), anyOf(equalTo("attributes.code"), equalTo("attributes.name")));
    }
  }

  @Test
  public void shouldReturn400OnAssignNotExistedDepartmentToUser() {
    String notExistedId = UUID.randomUUID().toString();
    Department department = post(createDepartment(null, "name1", "code"));
    String errorMessage = assignDepartmentWithError(testUser, department.getId(), notExistedId);
    assertThat(errorMessage, containsString("Cannot set users.departments = " + notExistedId));
  }

  @Test
  public void shouldUpdateDepartmentOnValidPut() {
    Department department = post(createDepartment(null, "name", "code"));
    department.getAttributes().setName("name new");
    department.getAttributes().setName("code new");
    Department actual = put(department);

    assertThat(actual.getId(), notNullValue());
    assertThat(actual.getType(), equalTo(department.getType()));
    assertThat(actual.getAttributes().getName(), equalTo(department.getAttributes().getName()));
    assertThat(actual.getAttributes().getCode(), equalTo(department.getAttributes().getCode()));
    assertThat(actual.getMetadata(), notNullValue());
  }

  @Test
  public void shouldReturn422OnPutWithDuplicateName() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.getAttributes().setName(dep1.getAttributes().getName());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this name already exists"));
  }

  @Test
  public void shouldReturn422OnPutWithDuplicateCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Department dep2 = post(createDepartment(null, "name2", "code2"));
    dep2.getAttributes().setCode(dep1.getAttributes().getCode());

    Errors errors = putWithError(dep2);
    assertThat(errors.getErrors().get(0).getMessage(), containsString("Department with this code already exists"));
  }

  @Test
  public void shouldReturn422OnPutWithEmptyNameAndCode() {
    Department dep1 = post(createDepartment(null, "name1", "code1"));
    Errors errors = putWithError(dep1.withAttributes(new DepartmentAttributes()));
    assertThat(errors.getErrors(), hasSize(2));
    for (Error error : errors.getErrors()) {
      assertThat(error.getMessage(), equalTo("may not be null"));
      assertThat(error.getParameters().get(0).getKey(), anyOf(equalTo("attributes.code"), equalTo("attributes.name")));
    }
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
    dep1.getAttributes().setUsageNumber(0);
    dep2.getAttributes().setUsageNumber(0);

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
    dep1.getAttributes().setUsageNumber(0);
    dep2.getAttributes().setUsageNumber(0);

    DepartmentCollection actual = getCollection("attributes.code==\"*1*\"");
    assertThat(actual, notNullValue());
    assertThat(actual.getDepartments(), hasSize(1));
    assertThat(actual.getTotalRecords(), equalTo(1));
    assertThat(actual.getDepartments(), hasItem(dep1));
  }

  @Test
  public void shouldReturnDepartmentCollectionWithUsageNumberOnGet() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    expected.getAttributes().setUsageNumber(1);
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
    expected.getAttributes().setUsageNumber(0);

    Department actual = getItem(id);
    assertThat(actual, equalTo(expected));
  }

  @Test
  public void shouldReturnDepartmentWithUsageNumberOnGetById() {
    String id = UUID.randomUUID().toString();
    Department expected = post(createDepartment(id, "name1", "code1"));
    assignDepartment(testUser, id);

    expected.getAttributes().setUsageNumber(1);

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

  private DepartmentCollection getCollection(String query) {
    return getWithOk(collectionEndpoint() + (query == null ? "" : "?query=" + query)).as(DepartmentCollection.class);
  }

  private Department getItem(String id) {
    return getWithOk(itemEndpoint(id)).as(Department.class);
  }

  private String getItemWithError(String id, int code) {
    return getWithStatus(itemEndpoint(id), code).asString();
  }

  protected Department post(Department department) {
    return postWithStatus(collectionEndpoint(), toJson(department), SC_CREATED, FAKE_TOKEN).as(Department.class);
  }

  protected Errors postWithError(Department department) {
    return postWithStatus(collectionEndpoint(), toJson(department), SC_UNPROCESSABLE_ENTITY, FAKE_TOKEN).as(Errors.class);
  }

  protected Department put(Department department) {
    putWithNoContent(itemEndpoint(department.getId()), toJson(department), FAKE_TOKEN);
    return getItem(department.getId());
  }

  protected Errors putWithError(Department department) {
    return putWithStatus(itemEndpoint(department.getId()), toJson(department), SC_UNPROCESSABLE_ENTITY, FAKE_TOKEN)
      .as(Errors.class);
  }

  private String deleteItemWithError(String id, int code) {
    return deleteWithStatus(itemEndpoint(id), code).asString();
  }

  protected void assignDepartment(User user, String... departmentIds) {
    user.setDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    putWithNoContent(USERS_ENDPOINT + "/" + user.getId(), toJson(user), FAKE_TOKEN);
  }

  protected String assignDepartmentWithError(User user, String... departmentIds) {
    user.setDepartments(new HashSet<>(Arrays.asList(departmentIds)));
    return putWithStatus(USERS_ENDPOINT + "/" + user.getId(), toJson(user), SC_BAD_REQUEST, FAKE_TOKEN).asString();
  }

  private Department createDepartment(String id, String name, String code) {
    return new Department()
      .withId(id)
      .withType(Department.Type.DEPARTMENTS)
      .withAttributes(
        new DepartmentAttributes()
          .withName(name)
          .withCode(code)
      );
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
