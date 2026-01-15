package org.folio.support;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

/**
 * Test helper class for Setting entity.
 * Uses Java 21 features with records-style value objects.
 */
@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Setting {
  String id;
  String scope;
  String key;
  Object value;
  String userId;
  @JsonProperty("_version")
  Integer version;
  Metadata metadata;

  /**
   * Creates a new Setting with updated value.
   *
   * @param newValue the new value to set
   * @return a new Setting instance with the updated value
   */
  public Setting withValue(Object newValue) {
    return Setting.builder()
      .id(this.id)
      .scope(this.scope)
      .key(this.key)
      .value(newValue)
      .userId(this.userId)
      .version(this.version)
      .metadata(this.metadata)
      .build();
  }

  /**
   * Creates a new Setting with updated key.
   *
   * @param newKey the new key to set
   * @return a new Setting instance with the updated key
   */
  public Setting withKey(String newKey) {
    return Setting.builder()
      .id(this.id)
      .scope(this.scope)
      .key(newKey)
      .value(this.value)
      .userId(this.userId)
      .version(this.version)
      .metadata(this.metadata)
      .build();
  }

  /**
   * Creates a new Setting with updated id.
   *
   * @param newId the new id to set
   * @return a new Setting instance with the updated id
   */
  public Setting withId(String newId) {
    return Setting.builder()
      .id(newId)
      .scope(this.scope)
      .key(this.key)
      .value(this.value)
      .userId(this.userId)
      .version(this.version)
      .metadata(this.metadata)
      .build();
  }

  /**
   * Creates a new Setting with updated _version.
   *
   * @param newVersion the new version to set
   * @return a new Setting instance with the updated _version
   */
  public Setting withVersion(int newVersion) {
    return Setting.builder()
      .id(this.id)
      .scope(this.scope)
      .key(this.key)
      .value(this.value)
      .userId(this.userId)
      .version(newVersion)
      .metadata(this.metadata)
      .build();
  }
}

