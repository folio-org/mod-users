package org.folio.support;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

import java.util.List;

/**
 * Test helper class for Settings collection.
 */
@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Settings {
  List<Setting> settings;
  Integer totalRecords;

  /**
   * Checks if the collection is empty.
   *
   * @return true if no settings are present
   */
  public boolean isEmpty() {
    return settings == null || settings.isEmpty();
  }

  /**
   * Gets the size of the settings collection.
   *
   * @return the number of settings
   */
  public int size() {
    return settings != null ? settings.size() : 0;
  }
}

