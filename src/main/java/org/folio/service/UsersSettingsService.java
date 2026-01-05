package org.folio.service;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static javax.ws.rs.core.Response.Status.BAD_REQUEST;
import static org.folio.rest.jaxrs.resource.UsersSettingsEntries.PutUsersSettingsEntriesByIdResponse.respond400WithTextPlain;
import static org.folio.rest.jaxrs.resource.UsersSettingsEntries.PutUsersSettingsEntriesByIdResponse.respond500WithTextPlain;
import static org.folio.support.UsersApiConstants.PROFILE_PICTURE_SETTING_KEY;
import static org.folio.support.UsersApiConstants.TABLE_NAME_SETTINGS;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.exceptions.UsersSettingsException;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.jaxrs.resource.UsersSettingsEntries;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.interfaces.Results;

import javax.ws.rs.core.Response;

public class UsersSettingsService {

  private static final Logger log = LogManager.getLogger(UsersSettingsService.class);
  public static final String SETTINGS_TABLE = "settings";

  public Future<Response> updateSetting(Conn conn, String id, Setting setting) {
    log.debug("updateSetting:: id: {}, setting: {}", id, setting);
    return conn.getById(SETTINGS_TABLE, id, Setting.class)
      .compose(oldSetting -> updateEntity(conn, id, oldSetting, setting));

  }

  public Future<Config> getProfilePictureConfigSetting(Conn conn) {
    var searchCriteria = new Criteria().addField("'key'").setOperation("=").setVal(PROFILE_PICTURE_SETTING_KEY);
    var criterion = new Criterion().addCriterion(searchCriteria).setLimit(new Limit(1));

    return conn
      .get(SETTINGS_TABLE, Setting.class, criterion)
      .compose(this::getFirstExceptionally)
      .map(Setting::getValue)
      .map(settingValue -> JsonObject.mapFrom(settingValue).mapTo(Config.class))
      .onSuccess(config -> log.debug("getProfilePictureConfigSetting:: found entity: {}", () -> config))
      .onFailure(err -> log.debug("getProfilePictureConfigSetting:: failed", err));
  }

  public Future<Config> createProfilePictureConfigSetting(Conn conn, Config config) {
    log.debug("createProfilePictureConfigSetting:: {}", () -> config);
    var id = UUID.randomUUID().toString();
    config.setConfigName(null);
    config.setMetadata(null);
    var settingValue = JsonObject.mapFrom(config).mapTo(Map.class);
    settingValue.remove("id");

    var setting = new Setting()
      .withId(id)
      .withKey(PROFILE_PICTURE_SETTING_KEY)
      .withScope(Setting.Scope.MOD_USERS)
      .withValue(settingValue);

    return conn
      .save(SETTINGS_TABLE, id, setting)
      .onSuccess(entityId -> log.debug("createProfilePictureConfigSetting:: parameter id: {}", entityId))
      .compose(entityId -> getProfilePictureConfigSetting(conn));
  }

  private Future<Response> updateEntity(Conn conn, String id, Setting prevValue, Setting newValue) {
    if (prevValue == null) {
      throw UsersSettingsException.notFoundById(id);
    }

    if (newValue.getId() != null && !newValue.getId().equals(id)) {
      throw new UsersSettingsException(getUpdateEntityIdMismatchError(newValue.getId()), BAD_REQUEST);
    }

    if (newValue.getKey().equals(PROFILE_PICTURE_SETTING_KEY)) {
      var oldConfig = JsonObject.mapFrom(prevValue.getValue()).mapTo(Config.class);
      var newConfig = JsonObject.mapFrom(newValue.getValue()).mapTo(Config.class);

      if (Objects.isNull(newConfig.getEncryptionKey()) ||
        !Objects.equals(oldConfig.getEncryptionKey(), newConfig.getEncryptionKey())) {
        return succeededFuture(respond400WithTextPlain("Cannot update the Encryption key"));
      }

      if (Objects.nonNull(newConfig.getMaxFileSize()) && newConfig.getMaxFileSize() > 10) {
        return succeededFuture(respond500WithTextPlain("Max file size should not exceed more than 10 megabytes"));
      }
    }

    return conn.update(TABLE_NAME_SETTINGS, newValue, id)
      .onSuccess(rows -> log.debug("updateSetting:: updated entity with id: {}", id))
      .map(rows -> UsersSettingsEntries.PutUsersSettingsEntriesByIdResponse.respond204());
  }

  private org.folio.rest.jaxrs.model.Error getUpdateEntityIdMismatchError(String id) {
    return new Error()
      .withMessage("ID of Setting for update not equal to id from path param: " + id)
      .withCode("id_mismatch_error")
      .withParameters(List.of(new Parameter().withKey("id").withValue(id)));
  }

  private Future<Setting> getFirstExceptionally(Results<Setting> results) {
    return results.getResults().isEmpty()
      ? failedFuture(UsersSettingsException.notFoundByKey(PROFILE_PICTURE_SETTING_KEY))
      : succeededFuture(results.getResults().getFirst());
  }
}
