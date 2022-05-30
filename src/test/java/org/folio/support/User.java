package org.folio.support;

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
  Personal personal;
  String patronGroup;
  TagList tags;
  Metadata metadata;
}
