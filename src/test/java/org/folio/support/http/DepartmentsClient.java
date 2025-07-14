package org.folio.support.http;

import static io.restassured.http.ContentType.JSON;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;

import io.restassured.http.Header;
import io.restassured.response.ValidatableResponse;

import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentCollection;

public class DepartmentsClient {
  private final RestAssuredCollectionApiClient<Department, DepartmentCollection> client;

  public DepartmentsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(okapiUrl.asURI("/departments"),
      defaultHeaders, Department.class, DepartmentCollection.class);
  }

  public Department createDepartment(Department department, Header userId, Header token) {
    return attemptCreateDepartment(department, userId, token)
      .statusCode(SC_CREATED)
      .extract()
      .as(Department.class);
  }

  public ValidatableResponse attemptCreateDepartment(Department dp, Header userId, Header token) {
    return client.initialSpecification()
      .header(userId)
      .header(token)
      .contentType(JSON)
      .when()
      .body(dp)
      .post()
      .then();
  }

  public DepartmentCollection getDepartments(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  public DepartmentCollection getAllDepartments() {
    return client.getAllRecords();
  }

  public void updateDepartment(Department department, Header userId, Header token) {
    attemptUpdateDepartment(department, userId, token).statusCode(SC_NO_CONTENT);
  }

  public ValidatableResponse attemptUpdateDepartment(Department dp, Header userId, Header token) {
    return attemptUpdateDepartment(dp.getId(), dp, userId, token);
  }

  public ValidatableResponse attemptUpdateDepartment(String id,
    Department dp, Header userId, Header token) {

    return client.initialSpecification()
      .header(userId)
      .header(token)
      .contentType(JSON)
      .when()
      .body(dp)
      .put("/{id}", id)
      .then();
  }

  public Department getDepartment(String id) {
    return client.getRecord(id);
  }

  public ValidatableResponse attemptGetDepartment(String id) {
    return client.attemptToGetRecord(id);
  }

  public void deleteDepartment(String id) {
    client.deleteRecord(id);
  }

  public ValidatableResponse attemptDeleteDepartment(String id) {
    return client.attemptToDeleteRecord(id);
  }

  public void deleteAllDepartments() {
    var allDepartments = getAllDepartments();
    allDepartments.getDepartments()
      .forEach(department -> client.deleteRecord(department.getId()));
  }
}
