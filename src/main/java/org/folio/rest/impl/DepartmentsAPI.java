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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.http.HttpStatus;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Department;
import org.folio.rest.jaxrs.model.DepartmentCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.resource.Departments;
import org.folio.rest.tools.utils.ValidationHelper;

public class DepartmentsAPI implements Departments {
  private static final Logger logger = LogManager.getLogger(DepartmentsAPI.class);

  private static final String DEPARTMENTS_TABLE_NAME = "departments";
  private static final String DEPARTMENTS_VIEW_NAME = "departments_view";

  private static final String ID_FIELD = "id";
  private static final String NAME_FIELD = "name";
  private static final String CODE_FIELD = "code";

  private static final String DUPLICATE_FIELD_MESSAGE = "Department with this %s already exists";

  @Validate
  @Override
  public void getDepartments(String query, String totalRecords, int offset, int limit,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {

    get(DEPARTMENTS_VIEW_NAME, Department.class, DepartmentCollection.class, query, offset, limit,
      okapiHeaders, vertxContext, GetDepartmentsResponse.class, resultHandler);
  }

  @Validate
  @Override
  public void postDepartments(Department entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {

    post(DEPARTMENTS_TABLE_NAME, entity, okapiHeaders, vertxContext, PostDepartmentsResponse.class, result ->
      handleUniqueConstraintViolation(result, entity, PostDepartmentsResponse::respond422WithApplicationJson, resultHandler)
    );
  }

  @Validate
  @Override
  public void putDepartmentsByDepartmentId(String departmentId, Department entity,
                                           Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {
    put(DEPARTMENTS_TABLE_NAME, entity, departmentId, okapiHeaders, vertxContext,
      PutDepartmentsByDepartmentIdResponse.class, result -> handleUniqueConstraintViolation(result, entity,
        PutDepartmentsByDepartmentIdResponse::respond422WithApplicationJson, resultHandler)
    );
  }

  @Validate
  @Override
  public void getDepartmentsByDepartmentId(String departmentId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {

    getById(DEPARTMENTS_VIEW_NAME, Department.class, departmentId, okapiHeaders, vertxContext,
      GetDepartmentsByDepartmentIdResponse.class, resultHandler);
  }

  @Validate
  @Override
  public void deleteDepartmentsByDepartmentId(String departmentId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> resultHandler, Context vertxContext) {

    deleteById(DEPARTMENTS_TABLE_NAME, departmentId, okapiHeaders, vertxContext,
      DeleteDepartmentsByDepartmentIdResponse.class, resultHandler);
  }

  static void handleUniqueConstraintViolation(AsyncResult<Response> result, Department entity,
      Function<Errors, Response> errorsMapFunction,
      Handler<AsyncResult<Response>> resultHandler) {

    AsyncResult<Response> finalResult = result;
    try {
      Errors error = null;
      if (result.succeeded() && result.result().getStatus() == HttpStatus.SC_UNPROCESSABLE_ENTITY) {
        String errorMessage = ((Errors) result.result().getEntity()).getErrors().iterator().next().getMessage();
        if (isDuplicateField(errorMessage, ID_FIELD)) {
          error = createDuplicateErrorMessage(ID_FIELD, entity.getId());
        } else if (isDuplicateField(errorMessage, NAME_FIELD)) {
          error = createDuplicateErrorMessage(NAME_FIELD, entity.getName());
        } else if (isDuplicateField(errorMessage, CODE_FIELD)) {
          error = createDuplicateErrorMessage(CODE_FIELD, entity.getCode());
        }
      }
      if (error != null) {
        finalResult = Future.succeededFuture(errorsMapFunction.apply(error));
      }
      resultHandler.handle(finalResult);
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      resultHandler.handle(finalResult);
    }
  }

  private static Errors createDuplicateErrorMessage(String fieldName, String value) {
    return ValidationHelper
      .createValidationErrorMessage(fieldName, value, String.format(DUPLICATE_FIELD_MESSAGE, fieldName));
  }

  private static boolean isDuplicateField(String errorMessage, String fieldName) {
    return errorMessage != null && errorMessage.matches(".*" + fieldName + ".*already exists.*");
  }
}
