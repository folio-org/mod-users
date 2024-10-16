package org.folio.support;

import java.util.Date;
import java.util.Map;
import java.time.ZonedDateTime;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;
import org.folio.rest.jaxrs.model.PreferredEmailCommunication;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class User {
  String id;
  String username;
  String barcode;
  Boolean active;
  String type;
  ZonedDateTime expirationDate;
  Personal personal;
  String patronGroup;
  TagList tags;
  Metadata metadata;
  Map<String, String> customFields;
  Set<PreferredEmailCommunication> preferredEmailCommunication;
  String externalSystemId;
  Date enrollmentDate;
}
