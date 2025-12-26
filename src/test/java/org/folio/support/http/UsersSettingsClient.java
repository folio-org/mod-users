package org.folio.support.http;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;

import io.restassured.response.ValidatableResponse;
import lombok.NonNull;
import org.folio.support.Setting;
import org.folio.support.Settings;

/**
 * HTTP client for Users Settings API integration tests.
 */
public class UsersSettingsClient {
  private final RestAssuredCollectionApiClient<Setting, Settings> client;

  public UsersSettingsClient(OkapiUrl okapiUrl, OkapiHeaders defaultHeaders) {
    client = new RestAssuredCollectionApiClient<>(
      okapiUrl.asURI("/users/settings"),
      defaultHeaders,
      Setting.class,
      Settings.class
    );
  }

  /**
   * Creates a new setting.
   *
   * @param setting the setting to create
   * @return the created setting
   */
  public Setting createSetting(@NonNull Setting setting) {
    return client.createRecord(setting);
  }

  /**
   * Attempts to create a setting and returns the response for validation.
   *
   * @param setting the setting to create
   * @return the validatable response
   */
  public ValidatableResponse attemptToCreateSetting(@NonNull Setting setting) {
    return client.attemptToCreateRecord(setting);
  }

  /**
   * Gets a setting by ID.
   *
   * @param id the setting ID
   * @return the setting
   */
  public Setting getSetting(String id) {
    return client.getRecord(id);
  }

  /**
   * Attempts to get a setting by ID and returns the response for validation.
   *
   * @param id the setting ID
   * @return the validatable response
   */
  public ValidatableResponse attemptToGetSetting(String id) {
    return client.attemptToGetRecord(id);
  }

  /**
   * Gets settings by CQL query.
   *
   * @param cqlQuery the CQL query
   * @return the settings collection
   */
  public Settings getSettings(String cqlQuery) {
    return client.getRecords(cqlQuery);
  }

  /**
   * Attempts to get settings by CQL query and returns the response for validation.
   *
   * @param cqlQuery the CQL query
   * @return the validatable response
   */
  public ValidatableResponse attemptToGetSettings(String cqlQuery) {
    return client.attemptToGetRecords(cqlQuery);
  }

  /**
   * Gets all settings.
   *
   * @return all settings
   */
  public Settings getAllSettings() {
    return client.getAllRecords();
  }

  /**
   * Updates a setting.
   *
   * @param setting the setting to update
   */
  public void updateSetting(@NonNull Setting setting) {
    attemptToUpdateSetting(setting)
      .statusCode(HTTP_NO_CONTENT);
  }

  /**
   * Attempts to update a setting and returns the response for validation.
   *
   * @param setting the setting to update
   * @return the validatable response
   */
  public ValidatableResponse attemptToUpdateSetting(@NonNull Setting setting) {
    return client.attemptToUpdateRecord(setting.getId(), setting);
  }

  /**
   * Attempts to update a setting by ID and returns the response for validation.
   *
   * @param id the setting ID
   * @param setting the setting to update
   * @return the validatable response
   */
  public ValidatableResponse attemptToUpdateSetting(String id, @NonNull Setting setting) {
    return client.attemptToUpdateRecord(id, setting);
  }

  /**
   * Deletes a setting by ID.
   *
   * @param id the setting ID
   */
  public void deleteSetting(String id) {
    client.deleteRecord(id);
  }

  /**
   * Attempts to delete a setting by ID and returns the response for validation.
   *
   * @param id the setting ID
   * @return the validatable response
   */
  public ValidatableResponse attemptToDeleteSetting(String id) {
    return client.attemptToDeleteRecord(id);
  }
}

