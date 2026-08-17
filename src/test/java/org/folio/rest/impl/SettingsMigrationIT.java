package org.folio.rest.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.ConfigurationEntry;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.support.http.UsersSettingsClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.NullSource;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import lombok.SneakyThrows;

class SettingsMigrationIT extends AbstractRestTestNoData {

  private static final String SETTINGS_TABLE = "settings";
  private static UsersSettingsClient settingsClient;

  @BeforeAll
  static void setUp() {
    settingsClient = new UsersSettingsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  void beforeEach() {
    deleteFromTable(SETTINGS_TABLE);
    assertTrue(getAllSettingsFromDatabase().isEmpty());
  }

  @AfterAll
  static void tearDown() {
    wireMockHelper.mockConfiguration(); // restore default configuration mock
  }

  @Test
  void settingsAreMigratedWhenModuleIsEnabled() {
    String configurationId = randomId();
    JsonArray value = new JsonArray(List.of(randomId(), randomId()));
    ConfigurationEntry suppressEditConfig = new ConfigurationEntry()
      .withId(configurationId)
      .withModule("@folio/users")
      .withConfigName("suppressEdit")
      .withValue(value.encode());

    wireMockHelper.mockConfiguration(List.of(suppressEditConfig));
    enableModule("19.6.0", "19.7.0"); // triggers migration

    JsonObject expectedSetting = new JsonObject()
      .put("id", configurationId)
      .put("scope", "mod-users")
      .put("key", "suppressEdit")
      .put("value", value)
      .put("_version", 1);

    assertEquals(1, getAllSettingsFromDatabase().size());
    assertEquals(expectedSetting, getSettingFromDatabaseAsJson(configurationId));

    // run migration again to verify that it is idempotent and does not create duplicates
    enableModule("19.6.0", "19.7.0");
    assertEquals(1, getAllSettingsFromDatabase().size());
    assertEquals(expectedSetting, getSettingFromDatabaseAsJson(configurationId));

    // verify that API also returns the same JSON representation of the setting
    assertEquals(expectedSetting, settingsClient.getSettingAsJson(configurationId));
  }

  @Test
  void settingsAreMigratedWheModuleFromIsNull() {
    wireMockHelper.mockConfiguration(List.of(suppressEditConfig()));
    enableModule(null, "19.7.0");

    assertEquals(1, getAllSettingsFromDatabase().size());
  }

  @Test
  void settingsAreNotMigratedWhenModuleFromIsAtThreshold() {
    wireMockHelper.mockConfiguration(List.of(suppressEditConfig()));
    enableModule("19.7.0", "19.7.0");

    assertTrue(getAllSettingsFromDatabase().isEmpty());
  }

  @Test
  void settingsAreNotMigratedWhenModuleFromIsAboveThreshold() {
    wireMockHelper.mockConfiguration(List.of(suppressEditConfig()));
    enableModule("19.8.0", "19.9.0");

    assertTrue(getAllSettingsFromDatabase().isEmpty());
  }

  @Test
  void settingsAreMigratedWhenModuleFromIsPreReleaseBelowThreshold() {
    wireMockHelper.mockConfiguration(List.of(suppressEditConfig()));
    enableModule("19.6.9-SNAPSHOT", "19.7.0");

    assertEquals(1, getAllSettingsFromDatabase().size());
  }

  @Test
  void settingsAreNotMigratedWhenConfigurationIsNotFound() {
    wireMockHelper.mockConfiguration(List.of());
    enableModule("19.6.0", "19.7.0");

    assertTrue(getAllSettingsFromDatabase().isEmpty());
  }

  @ParameterizedTest
  @EmptySource
  @NullSource
  void settingsAreNotMigratedWhenConfigurationValueIsNullOrEmpty(String configurationValue) {
    ConfigurationEntry config = new ConfigurationEntry()
      .withId(randomId())
      .withModule("@folio/users")
      .withConfigName("suppressEdit")
      .withValue(configurationValue);

    wireMockHelper.mockConfiguration(List.of(config));
    enableModule("19.6.0", "19.7.0");

    assertTrue(getAllSettingsFromDatabase().isEmpty());
  }

  private ConfigurationEntry suppressEditConfig() {
    return new ConfigurationEntry()
      .withId(randomId())
      .withModule("@folio/users")
      .withConfigName("suppressEdit")
      .withValue(new JsonArray(List.of(randomId(), randomId())).encode());
  }

  @SneakyThrows
  private void enableModule(String versionFrom, String versionTo) {
    String moduleFrom = versionFrom == null ? null : "mod-users-" + versionFrom;
    String moduleTo = versionTo == null ? null : "mod-users-" + versionTo;
    wait(module.migrateModule(okapiHeaders, moduleFrom, moduleTo, false, false));
  }

  @SneakyThrows
  private List<Setting> getAllSettingsFromDatabase() {
    CQLWrapper cqlAllRecords = new CQLWrapper(new CQL2PgJSON("jsonb"), "cql.allRecords=1");
    return wait(postgresClient.get(SETTINGS_TABLE, Setting.class, cqlAllRecords, true))
      .getResults();
  }

  @SneakyThrows
  private JsonObject getSettingFromDatabaseAsJson(String settingId) {
    return new JsonObject(wait(postgresClient.getByIdAsString(SETTINGS_TABLE, settingId)));
  }

  private static String randomId() {
    return UUID.randomUUID().toString();
  }

  @SneakyThrows
  private <T> T wait(Future<T> future) {
    return future.toCompletionStage()
      .toCompletableFuture()
      .get(10, TimeUnit.SECONDS);
  }
}
