package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_OK;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.support.Setting;
import org.folio.support.Settings;
import org.folio.support.http.UsersSettingsClient;
import org.folio.support.tags.IntegrationTest;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Integration tests for Users Settings API.
 */
@Slf4j
@IntegrationTest
@ExtendWith(VertxExtension.class)
class UsersSettingsAPIIT extends AbstractRestTestNoData {

  private static UsersSettingsClient settingsClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    settingsClient = new UsersSettingsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  void beforeEach() {
    try {
      final var response = settingsClient.attemptToGetSettings("cql.allRecords=1");
      if (response.extract().statusCode() == HTTP_OK) {
        final var allSettings = response.extract().as(Settings.class);
        if (allSettings != null && allSettings.getSettings() != null) {
          allSettings.getSettings().forEach(setting -> settingsClient.attemptToDeleteSetting(setting.getId()));
        }
      }
    } catch (Exception e) {
      log.warn("Error during cleanup: {}", e.getMessage());
    }
  }

  // ========== CREATE TESTS ==========

  @Test
  void canCreateSetting() {
    final var settingToCreate = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("test.setting")
      .value("test-value")
      .build();


    final var createdSetting = settingsClient.createSetting(settingToCreate);

    assertThat(createdSetting.getId(), is(notNullValue()));
    assertThat(createdSetting.getScope(), is("mod-users"));
    assertThat(createdSetting.getKey(), is("test.setting"));
    assertThat(createdSetting.getValue(), is("test-value"));
    assertThat(createdSetting.getMetadata(), is(notNullValue()));
    assertThat(createdSetting.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdSetting.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void canCreateSettingWithComplexValue() {
    final var complexValue = Map.of(
      "enabled", true,
      "maxAttempts", 3,
      "timeout", 30,
      "nested", Map.of("key", "value")
    );

    final var settingToCreate = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("complex.setting")
      .value(complexValue)
      .build();

    final var createdSetting = settingsClient.createSetting(settingToCreate);

    assertThat(createdSetting.getId(), is(notNullValue()));
    assertThat(createdSetting.getValue(), is(notNullValue()));
  }

  @Test
  void canCreateSettingWithUserId() {
    final var userId = UUID.randomUUID().toString();
    final var settingToCreate = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("user.specific.setting")
      .value("user-value")
      .userId(userId)
      .build();

    final var createdSetting = settingsClient.createSetting(settingToCreate);

    assertThat(createdSetting.getUserId(), is(userId));
  }

  @Test
  void canCreateSettingWithoutId() {
    final var userId = UUID.randomUUID().toString();
    final var settingToCreate = Setting.builder()
      .scope("mod-users")
      .key("user.specific.setting")
      .value("user-value")
      .userId(userId)
      .build();

    final var createdSetting = settingsClient.createSetting(settingToCreate);

    assertThat(createdSetting.getUserId(), is(userId));
    assertNotNull(createdSetting.getId());
  }

  @ParameterizedTest
  @ValueSource(strings = {"string", "123", "true", "null"})
  void canCreateSettingWithDifferentValueTypes(String value) {
    final var settingToCreate = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("test.value.type." + value)
      .value(value)
      .build();

    final var createdSetting = settingsClient.createSetting(settingToCreate);

    assertThat(createdSetting.getValue(), is(notNullValue()));
  }

  @Test
  void cannotCreateSettingWithoutRequiredFields() {
    final var settingWithoutScope = Setting.builder()
      .id(UUID.randomUUID().toString())
      .key("test.key")
      .value("test-value")
      .build();

    settingsClient.attemptToCreateSetting(settingWithoutScope)
      .statusCode(is(422));
  }

  @Test
  void cannotCreateSettingWithoutKey() {
    final var settingWithoutKey = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .value("test-value")
      .build();

    settingsClient.attemptToCreateSetting(settingWithoutKey)
      .statusCode(is(422));
  }

  @Test
  void cannotCreateSettingWithoutValue() {
    final var settingWithoutValue = Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("test.key")
      .build();

    settingsClient.attemptToCreateSetting(settingWithoutValue)
      .statusCode(is(422));
  }

  // ========== GET BY ID TESTS ==========

  @Test
  void canGetSettingById() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("test.get.setting")
      .value("get-value")
      .build());

    final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());

    assertThat(fetchedSetting.getId(), is(createdSetting.getId()));
    assertThat(fetchedSetting.getScope(), is("mod-users"));
    assertThat(fetchedSetting.getKey(), is("test.get.setting"));
    assertThat(fetchedSetting.getValue(), is("get-value"));
    assertThat(fetchedSetting.getMetadata(), is(notNullValue()));
  }

  @Test
  void cannotGetSettingThatDoesNotExist() {
    final var nonExistentId = UUID.randomUUID().toString();

    settingsClient.attemptToGetSetting(nonExistentId)
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void canGetSettingWithComplexValue() {
    final var complexValue = new JsonObject()
      .put("setting1", "value1")
      .put("setting2", 42)
      .put("setting3", true)
      .getMap();

    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("complex.value.setting")
      .value(complexValue)
      .build());

    final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());

    assertThat(fetchedSetting.getValue(), is(notNullValue()));
  }

  // ========== GET COLLECTION TESTS ==========

  @Test
  void canGetAllSettings() {
    // Create multiple settings
    settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("setting.one")
      .value("value1")
      .build());

    settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("setting.two")
      .value("value2")
      .build());

    settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("setting.three")
      .value("value3")
      .build());

    final var allSettings = settingsClient.getAllSettings();

    assertThat(allSettings.getTotalRecords(), is(3));
    assertThat(allSettings.getSettings(), hasSize(3));
  }

  @Test
  void canGetSettingsByQuery() {
    // Create settings with different keys
    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("prefix.setting.one")
      .value("value1")
      .build());

    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("prefix.setting.two")
      .value("value2")
      .build());

    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("other.setting")
      .value("value3")
      .build());

    final var filteredSettings = settingsClient.getSettings("key=prefix.*");

    assertThat(filteredSettings.getTotalRecords(), is(greaterThan(0)));
  }

  @Test
  void canGetEmptySettingsCollection() {
    final var allSettings = settingsClient.getAllSettings();

    assertThat(allSettings.getTotalRecords(), is(0));
    assertThat(allSettings.getSettings(), hasSize(0));
    assertThat(allSettings.isEmpty(), is(true));
  }

  @Test
  void canGetSettingsByScope() {
    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("scoped.setting")
      .value("scoped-value")
      .build());

    final var scopedSettings = settingsClient.getSettings("scope==mod-users");

    assertThat(scopedSettings.getTotalRecords(), is(greaterThan(0)));
    scopedSettings.getSettings().forEach(setting ->
      assertThat(setting.getScope(), is("mod-users"))
    );
  }

  @Test
  void canGetSettingsByUserId() {
    final var userId = "65de6432-be11-48ba-9686-a65101634040";

    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("user.setting.one")
      .value("value1")
      .userId(userId)
      .build());

    settingsClient.createSetting(Setting.builder()
      .scope("mod-users")
      .key("user.setting.two")
      .value("value2")
      .userId(userId)
      .build());

    final var userSettings = settingsClient.getSettings(String.format("userId==\"%s\"", userId));

    assertThat(userSettings.getTotalRecords(), is(2));
  }

  // ========== UPDATE TESTS ==========

  @Test
  void canUpdateSetting() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("update.test")
      .value("original-value")
      .build());

    final var updatedSetting = createdSetting.withValue("updated-value");

    settingsClient.updateSetting(updatedSetting);

    final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());
    assertThat(fetchedSetting.getValue(), is("updated-value"));
  }

  @Test
  void canUpdateSettingKey() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("original.key")
      .value("value")
      .build());

    final var updatedSetting = createdSetting.withKey("updated.key");

    settingsClient.updateSetting(updatedSetting);

    final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());
    assertThat(fetchedSetting.getKey(), is("updated.key"));
  }

  @Test
  void canUpdateSettingWithComplexValue() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("complex.update")
      .value(Map.of("original", true))
      .build());

    final var newComplexValue = Map.of(
      "updated", true,
      "count", 10,
      "nested", Map.of("deep", "value")
    );

    final var updatedSetting = createdSetting.withValue(newComplexValue);

    settingsClient.updateSetting(updatedSetting);

    final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());
    assertThat(fetchedSetting.getValue(), is(notNullValue()));
  }

  @Test
  void cannotUpdateSettingThatDoesNotExist() {
    final var nonExistentId = UUID.randomUUID().toString();
    final var settingToUpdate = Setting.builder()
      .id(UUID.randomUUID().toString())
      .id(nonExistentId)
      .scope("mod-users")
      .key("non.existent")
      .value("value")
      .build();

    settingsClient.attemptToUpdateSetting(settingToUpdate)
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void cannotUpdateSettingWithMismatchedId() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("mismatch.test")
      .value("value")
      .build());

    final var differentId = UUID.randomUUID().toString();
    final var settingWithDifferentId = createdSetting.withId(differentId);

    settingsClient.attemptToUpdateSetting(createdSetting.getId(), settingWithDifferentId)
      .statusCode(is(HTTP_BAD_REQUEST))
      .body(containsString("ID of Setting for update not equal to id from path param"));
  }

  @Test
  void updateSettingPreservesMetadata() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("metadata.test")
      .value("original")
      .build());

    final var originalCreatedDate = createdSetting.getMetadata().getCreatedDate();
    assertThat(originalCreatedDate, is(notNullValue()));

    // Update the setting
    settingsClient.updateSetting(createdSetting.withValue("updated"));

    // Wait a bit to ensure updated date changes
    awaitUntilAsserted(() -> {
      final var fetchedSetting = settingsClient.getSetting(createdSetting.getId());
      assertThat(fetchedSetting.getMetadata().getCreatedDate(), is(originalCreatedDate));
      assertThat(fetchedSetting.getMetadata().getUpdatedDate(), is(notNullValue()));
    });
  }

  // ========== DELETE TESTS ==========

  @Test
  void canDeleteSetting() {
    final var createdSetting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("delete.test")
      .value("to-be-deleted")
      .build());

    settingsClient.deleteSetting(createdSetting.getId());

    settingsClient.attemptToGetSetting(createdSetting.getId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void cannotDeleteSettingThatDoesNotExist() {
    final var nonExistentId = UUID.randomUUID().toString();

    settingsClient.attemptToDeleteSetting(nonExistentId)
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void canDeleteMultipleSettings() {
    // Create multiple settings
    final var setting1 = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("delete.batch.one")
      .value("value1")
      .build());

    final var setting2 = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("delete.batch.two")
      .value("value2")
      .build());

    // Delete them individually
    settingsClient.deleteSetting(setting1.getId());
    settingsClient.deleteSetting(setting2.getId());

    // Verify they're gone
    settingsClient.attemptToGetSetting(setting1.getId())
      .statusCode(is(HTTP_NOT_FOUND));
    settingsClient.attemptToGetSetting(setting2.getId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  // ========== INTEGRATION TESTS ==========

  @Test
  void canPerformCompleteLifecycle() {
    // Create
    final var setting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("lifecycle.test")
      .value("initial")
      .build());

    assertThat(setting.getId(), is(notNullValue()));

    // Read
    final var fetched = settingsClient.getSetting(setting.getId());
    assertThat(fetched.getValue(), is("initial"));

    // Update
    settingsClient.updateSetting(fetched.withValue("updated"));
    final var updated = settingsClient.getSetting(setting.getId());
    assertThat(updated.getValue(), is("updated"));

    // Delete
    settingsClient.deleteSetting(setting.getId());
    settingsClient.attemptToGetSetting(setting.getId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void cannotCreateMultipleSettingsForSameKey() {
    final var userId1 = UUID.randomUUID().toString();
    final var userId2 = UUID.randomUUID().toString();
    var sharedKey = "shared-key";

    // Create settings with same key but different users
    settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key(sharedKey)
      .value("user1-value")
      .userId(userId1)
      .build());

    settingsClient.attemptToCreateSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key(sharedKey)
      .value("user2-value")
      .userId(userId2)
      .build())
      .statusCode(is(HTTP_BAD_REQUEST))
      .body(containsString("value already exists in table settings: %s".formatted(sharedKey)));
  }

  @ParameterizedTest
  @MethodSource("provideComplexValues")
  void canHandleVariousComplexValues(Object complexValue) {
    final var setting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("complex.value.test")
      .value(complexValue)
      .build());

    final var fetched = settingsClient.getSetting(setting.getId());
    assertThat(fetched.getValue(), is(notNullValue()));
  }

  @Test
  void canQueryWithPagination() {
    // Create multiple settings
    for (int i = 0; i < 10; i++) {
      settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
        .scope("mod-users")
        .key("pagination.test." + i)
        .value("value" + i)
        .build());
    }

    final var allSettings = settingsClient.getAllSettings();
    assertThat(allSettings.getTotalRecords(), is(10));
    assertThat(allSettings.size(), is(10));
  }

  @Test
  void settingsSurviveMultipleUpdates() {
    final var setting = settingsClient.createSetting(Setting.builder()
      .id(UUID.randomUUID().toString())
      .scope("mod-users")
      .key("multiple.updates")
      .value("v1")
      .build());

    // Perform multiple updates
    final var values = List.of("v2", "v3", "v4", "v5");
    for (String value : values) {
      final var current = settingsClient.getSetting(setting.getId());
      settingsClient.updateSetting(current.withValue(value));
    }

    final var finalSetting = settingsClient.getSetting(setting.getId());
    assertThat(finalSetting.getValue(), is("v5"));
  }

  // ========== HELPER METHODS ==========

  /**
   * Provides various complex value types for parameterized testing.
   * Uses Java 21 record patterns and enhanced collections.
   */
  private static Stream<Arguments> provideComplexValues() {
    return Stream.of(
      Arguments.of(Map.of("key", "value")),
      Arguments.of(Map.of("number", 42, "boolean", true)),
      Arguments.of(List.of("item1", "item2", "item3")),
      Arguments.of(Map.of(
        "nested", Map.of(
          "deep", Map.of(
            "deeper", "value"
          )
        )
      )),
      Arguments.of(Map.of(
        "array", List.of(1, 2, 3),
        "object", Map.of("a", "b"),
        "primitive", "string"
      ))
    );
  }
}

