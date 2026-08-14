package org.folio.service;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.integration.http.HttpClientFactory.getHttpClient;

import java.util.Map;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.client.ConfigurationClient;
import org.folio.client.impl.ConfigurationClientImpl;
import org.folio.repository.SettingsRepository;
import org.folio.rest.jaxrs.model.ConfigurationEntry;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;

public class SettingsMigrationService {

  private static final Logger log = LogManager.getLogger(SettingsMigrationService.class);

  private static final String USERS_MODULE = "@folio/users";
  private static final String SUPPRESS_EDIT_CONFIG_NAME = "suppressEdit";
  private static final String DEFAULT_SCOPE = Setting.Scope.MOD_USERS.value();
  private static final Function<String, Object> JSON_ARRAY_TO_LIST_TRANSFORMER =
    jsonArrayString -> new JsonArray(jsonArrayString).stream().toList();

  private final ConfigurationClient configurationClient;
  private final SettingsRepository settingsRepository;

  public SettingsMigrationService(Context vertxContext, Map<String, String> okapiHeaders) {
    this.configurationClient = new ConfigurationClientImpl(getHttpClient(vertxContext.owner()), okapiHeaders);
    this.settingsRepository = new SettingsRepository(vertxContext, okapiHeaders);
  }

  public Future<Void> migrateSettings() {
    return migrateSetting(USERS_MODULE, SUPPRESS_EDIT_CONFIG_NAME, JSON_ARRAY_TO_LIST_TRANSFORMER)
      .onSuccess(v -> log.info("migrateSettings:: migration completed successfully"))
      .onFailure(t -> log.error("migrateSettings:: migration failed", t));
  }

  private Future<Void> migrateSetting(String module, String configName,
    Function<String, Object> valueTransformer) {

    log.info("migrateSetting:: module={}, configName={}", module, configName);
    return doesSettingExist(DEFAULT_SCOPE, configName)
      .compose(settingExists -> migrateSetting(settingExists, module, configName, valueTransformer));
  }

  private Future<Boolean> doesSettingExist(String scope, String key) {
    log.info("doesSettingExist:: checking if setting exists: scope={}, key={}", scope, key);

    Criteria scopeCriterion = new Criteria()
      .addField("'scope'")
      .setOperation("=")
      .setVal(scope)
      .setJSONB(true);

    Criteria keyCriterion = new Criteria()
      .addField("'key'")
      .setOperation("=")
      .setVal(key)
      .setJSONB(true);

    Criterion criterion = new Criterion()
      .addCriterion(scopeCriterion, "AND", keyCriterion)
      .setLimit(new Limit(1));

    return settingsRepository.get(criterion)
      .map(settings -> !settings.isEmpty());
  }

  private Future<Void> migrateSetting(boolean settingExists, String module, String configName,
    Function<String, Object> valueTransformer) {

    if (settingExists) {
      log.info("migrateSetting:: setting already exists, skipping migration");
      return succeededFuture();
    }
    log.info("migrateSetting:: setting does not exist, proceeding with migration");

    return configurationClient.getConfiguration(module, configName)
      .compose(config -> saveSetting(config, valueTransformer));
  }

  private Future<Void> saveSetting(ConfigurationEntry config, Function<String, Object> valueTransformer) {
    if (config == null) {
      log.info("saveSetting:: configuration not found, skipping migration");
      return succeededFuture();
    }

    String value = config.getValue();
    if (value == null || value.isEmpty()) {
      log.info("saveSetting:: configuration value is null or empty, skipping migration");
      return succeededFuture();
    }

    Setting setting = new Setting()
      .withId(config.getId())
      .withScope(Setting.Scope.MOD_USERS)
      .withKey(config.getConfigName())
      .withValue(valueTransformer.apply(value));

    log.info("saveSetting:: saving setting: scope={}, key={}", setting::getScope, setting::getKey);

    return settingsRepository.save(setting.getId(), setting)
      .mapEmpty();
  }

}
