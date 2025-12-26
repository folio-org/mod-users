package org.folio.rest.impl;

import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.Response.Status.BAD_REQUEST;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;

import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;
import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.exceptions.UsersSettingsException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.jaxrs.model.SettingCollection;
import org.folio.rest.jaxrs.resource.UsersSettings;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.PgUtil;

public class UsersSettingsAPI implements UsersSettings {

  private static final Logger logger = LogManager.getLogger(UsersSettingsAPI.class);

  public static final String SETTINGS_TABLE = "settings";

  @Override
  public void getUsersSettings(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.get(SETTINGS_TABLE, Setting.class, SettingCollection.class, query, offset, limit,
      okapiHeaders, context, GetUsersSettingsResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void postUsersSettings(Setting setting, Map<String, String> okapiHeaders,
                                 Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.post(SETTINGS_TABLE, setting, okapiHeaders, context, PostUsersSettingsResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler)
      .onFailure(e -> logger.error("Failed to create setting", e));
  }

  @Override
  public void getUsersSettingsById(String id, Map<String, String> okapiHeaders,
                                    Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.getById(SETTINGS_TABLE, Setting.class, id, okapiHeaders, context, GetUsersSettingsByIdResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void deleteUsersSettingsById(String id, Map<String, String> okapiHeaders,
                                       Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.deleteById(SETTINGS_TABLE, id, okapiHeaders, context, DeleteUsersSettingsByIdResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void putUsersSettingsById(String id, Setting entity, Map<String, String> okapiHeaders,
                                    Handler<AsyncResult<Response>> resultHandler, Context context) {
    var postgresClient = PgUtil.postgresClient(context, okapiHeaders);
    postgresClient.withTrans(conn -> conn.getById(SETTINGS_TABLE, id)
      .compose(oldValue -> updateEntity(conn, id, oldValue, entity)))
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  private Future<Response> updateEntity(Conn conn, String id, JsonObject prevValue, Setting newValue) {
    if (prevValue == null) {
      throw UsersSettingsException.notFoundById(id);
    }

    if (newValue.getId() != null && !newValue.getId().equals(id)) {
      throw new UsersSettingsException(getUpdateEntityIdMismatchError(newValue.getId()), BAD_REQUEST);
    }

    return conn.update(SETTINGS_TABLE, newValue, id)
      .map(updatedRs -> PutUsersSettingsByIdResponse.respond204());
  }

  private Error getUpdateEntityIdMismatchError(String id) {
    return new Error()
      .withMessage("ID of Setting for update not equal to id from path param: " + id)
      .withCode("id_mismatch_error")
      .withParameters(List.of(new Parameter().withKey("id").withValue(id)));
  }

  private Response handleError(Throwable throwable) {
    if (throwable instanceof UsersSettingsException settingsException) {
      logger.debug("handleError:: Handling UsersSettingsException: ", throwable);
      return settingsException.buildErrorResponse();
    }

    logger.warn("handleError:: Handling unexpected error: ", throwable);
    var error = new Error()
      .withCode("settings_error")
      .withType(throwable.getClass().getSimpleName())
      .withMessage("Unexpected error occurred: " + throwable.getMessage());

    return Response.status(INTERNAL_SERVER_ERROR)
      .header(CONTENT_TYPE, APPLICATION_JSON)
      .entity(error)
      .build();
  }
}
