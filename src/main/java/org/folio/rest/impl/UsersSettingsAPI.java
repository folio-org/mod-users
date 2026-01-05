package org.folio.rest.impl;

import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static org.folio.support.UsersApiConstants.TABLE_NAME_SETTINGS;

import javax.ws.rs.core.Response;
import java.util.Map;
import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.exceptions.UsersSettingsException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.jaxrs.model.SettingCollection;
import org.folio.rest.jaxrs.resource.UsersSettingsEntries;
import org.folio.rest.persist.PgUtil;
import org.folio.service.UsersSettingsService;

public class UsersSettingsAPI implements UsersSettingsEntries {

  private static final Logger logger = LogManager.getLogger(UsersSettingsAPI.class);

  private final UsersSettingsService settingsService;

  public UsersSettingsAPI() {
    this.settingsService = new UsersSettingsService();
  }

  @Override
  public void getUsersSettingsEntries(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.get(TABLE_NAME_SETTINGS, Setting.class, SettingCollection.class, query, offset, limit,
      okapiHeaders, context, GetUsersSettingsEntriesResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void postUsersSettingsEntries(Setting setting, Map<String, String> okapiHeaders,
                                       Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.post(TABLE_NAME_SETTINGS, setting, okapiHeaders, context, PostUsersSettingsEntriesResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler)
      .onFailure(e -> logger.error("Failed to create setting", e));
  }

  @Override
  public void getUsersSettingsEntriesById(String id, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.getById(TABLE_NAME_SETTINGS, Setting.class, id, okapiHeaders, context, GetUsersSettingsEntriesByIdResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void deleteUsersSettingsEntriesById(String id, Map<String, String> okapiHeaders,
                                             Handler<AsyncResult<Response>> resultHandler, Context context) {
    PgUtil.deleteById(TABLE_NAME_SETTINGS, id, okapiHeaders, context, DeleteUsersSettingsEntriesByIdResponse.class)
      .otherwise(this::handleError)
      .onComplete(resultHandler);
  }

  @Override
  public void putUsersSettingsEntriesById(String id, Setting entity, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> resultHandler, Context context) {
    var postgresClient = PgUtil.postgresClient(context, okapiHeaders);
    postgresClient.withTrans(conn -> settingsService.updateSetting(conn, id, entity))
      .otherwise(this::handleError)
      .onComplete(resultHandler);
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

