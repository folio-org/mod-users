package org.folio.rest.impl;

import static org.folio.rest.persist.PgUtil.deleteById;
import static org.folio.rest.persist.PgUtil.get;
import static org.folio.rest.persist.PgUtil.getById;
import static org.folio.rest.persist.PgUtil.post;
import static org.folio.rest.persist.PgUtil.put;

import java.util.Map;
import java.util.function.Function;

import javax.ws.rs.core.Response;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.apache.http.HttpStatus;
import org.jsoup.Jsoup;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentAttributes;
import org.folio.rest.jaxrs.model.DepartmentCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.resource.Departments;
import org.folio.rest.tools.utils.ValidationHelper;

public class DepartmentsAPI implements Departments {

  private static final String DEPARTMENTS_TABLE_NAME = "departments";
  private static final String DEPARTMENTS_VIEW_NAME = "departments_view";

  private static final String ID_FIELD = "id";
  private static final String NAME_FIELD = "name";
  private static final String CODE_FIELD = "code";

  private static final String DUPLICATE_FIELD_MESSAGE = "Department with this %s already exists";

  @Validate
  @Override
  public void getDepartments(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    get(DEPARTMENTS_VIEW_NAME, Department.class, DepartmentCollection.class, query, offset, limit,
      okapiHeaders, vertxContext, GetDepartmentsResponse.class, resultHandler);
  }

  @Validate
  @Override
  public void postDepartments(String lang, Department entity, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    post(DEPARTMENTS_TABLE_NAME, cleanUp(entity), okapiHeaders, vertxContext, PostDepartmentsResponse.class, result ->
      handleUniqueConstraintViolation(result, entity, PostDepartmentsResponse::respond422WithApplicationJson, resultHandler)
    );
  }

  @Validate
  @Override
  public void putDepartmentsByDepartmentId(String departmentId, String lang, Department entity,
                                           Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    put(DEPARTMENTS_TABLE_NAME, cleanUp(entity), departmentId, okapiHeaders, vertxContext,
      PutDepartmentsByDepartmentIdResponse.class, result -> handleUniqueConstraintViolation(result, entity,
        PutDepartmentsByDepartmentIdResponse::respond422WithApplicationJson, resultHandler)
    );
  }

  @Validate
  @Override
  public void getDepartmentsByDepartmentId(String departmentId, String lang, Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    getById(DEPARTMENTS_VIEW_NAME, Department.class, departmentId, okapiHeaders, vertxContext,
      GetDepartmentsByDepartmentIdResponse.class, resultHandler);
  }

  @Validate
  @Override
  public void deleteDepartmentsByDepartmentId(String departmentId, String lang, Map<String, String> okapiHeaders,
                                              Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    deleteById(DEPARTMENTS_TABLE_NAME, departmentId, okapiHeaders, vertxContext,
      DeleteDepartmentsByDepartmentIdResponse.class, resultHandler);
  }

  private Department cleanUp(Department entity) {
    DepartmentAttributes attributes = entity.getAttributes();
    attributes.setName(Jsoup.parse(attributes.getName()).text());
    attributes.setCode(Jsoup.parse(attributes.getCode()).text());
    return entity;
  }

  private void handleUniqueConstraintViolation(AsyncResult<Response> result, Department entity,
                                               Function<Errors, Response> errorsMapFunction,
                                               Handler<AsyncResult<Response>> resultHandler) {
    Errors error = null;
    if (result.succeeded() && result.result().getStatus() == HttpStatus.SC_UNPROCESSABLE_ENTITY) {
      String errorMessage = ((Errors) result.result().getEntity()).getErrors().iterator().next().getMessage();
      if (isDuplicateField(errorMessage, ID_FIELD)) {
        error = createDuplicateErrorMessage(ID_FIELD, entity.getId());
      } else if (isDuplicateField(errorMessage, NAME_FIELD)) {
        error = createDuplicateErrorMessage(NAME_FIELD, entity.getAttributes().getName());
      } else if (isDuplicateField(errorMessage, CODE_FIELD)) {
        error = createDuplicateErrorMessage(CODE_FIELD, entity.getAttributes().getCode());
      }
    }
    resultHandler.handle(
      error != null
        ? Future.succeededFuture(errorsMapFunction.apply(error))
        : result
    );
  }

  private Errors createDuplicateErrorMessage(String fieldName, String value) {
    return ValidationHelper
      .createValidationErrorMessage(fieldName, value, String.format(DUPLICATE_FIELD_MESSAGE, fieldName));
  }

  private boolean isDuplicateField(String errorMessage, String fieldName) {
    return errorMessage != null && errorMessage.matches(".*" + fieldName + ".*already exists.*");
  }
}
