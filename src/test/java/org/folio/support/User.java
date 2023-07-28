package org.folio.support;

import java.util.Map;
import java.time.ZonedDateTime;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class User {
  String id;
  String username;
  String barcode;
  Boolean active;
  ZonedDateTime expirationDate;
  Personal personal;
  String patronGroup;
  TagList tags;
  Metadata metadata;
  String type;
  Map<String, String> customFields;
}
