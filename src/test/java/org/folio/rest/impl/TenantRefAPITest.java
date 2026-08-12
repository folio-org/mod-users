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
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.UsersSettingsClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import lombok.SneakyThrows;

class TenantRefAPITest extends AbstractRestTestNoData {

  private static final String SETTINGS_TABLE = "settings";
  private static UsersSettingsClient settingsClient;

  @BeforeAll
  static void setUp() {
    settingsClient = new UsersSettingsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  void beforeEach() {
    deleteFromTable(SETTINGS_TABLE);
  }

  @AfterAll
  static void tearDown() {
    wireMockHelper.mockConfiguration(); // restore default mock
  }

  @Test
  void suppressEditSettingIsMigratedWhenModuleIsEnabled() {
    assertTrue(getAllSettingsFromDatabase().isEmpty());

    String configurationId = randomId();
    JsonArray value = new JsonArray(List.of(randomId(), randomId()));
    ConfigurationEntry suppressEditConfig = new ConfigurationEntry()
      .withId(configurationId)
      .withModule("@folio/users")
      .withConfigName("suppressEdit")
      .withValue(value.encode());

    wireMockHelper.mockConfiguration(List.of(suppressEditConfig));
    enableModule(); // triggers migration

    JsonObject expectedSetting = new JsonObject()
      .put("id", configurationId)
      .put("scope", "mod-users")
      .put("key", "suppressEdit")
      .put("value", value)
      .put("_version", 1);

    assertEquals(1, getAllSettingsFromDatabase().size());
    assertEquals(expectedSetting, getSettingFromDatabaseAsJson(configurationId));

    // run migration again to verify that it is idempotent and does not create duplicates
    enableModule();
    assertEquals(1, getAllSettingsFromDatabase().size());
    assertEquals(expectedSetting, getSettingFromDatabaseAsJson(configurationId));

    // verify that API also returns the same JSON representation of the setting
    assertEquals(expectedSetting, settingsClient.getSettingAsJson(configurationId));
  }

  @SneakyThrows
  private void enableModule() {
    String wireMockUrl = "http://localhost:" + wireMockServer.port();
    OkapiHeaders customHeaders = new OkapiHeaders(wireMockUrl, okapiHeaders.getTenantId(),
      okapiHeaders.getToken());
    wait(module.migrateModule(customHeaders, "19.7.0", false, false));
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
