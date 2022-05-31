package org.folio.support;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class CustomField {
  String id;
  String name;
  Boolean visible;
  Boolean required;
  String helpText;
  String entityType;
  String type;
  Integer order;
}
