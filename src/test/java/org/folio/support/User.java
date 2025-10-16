package org.folio.support;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.time.ZonedDateTime;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;
import org.folio.rest.jaxrs.model.Department;
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
  Set<String> departments;
  Set<PreferredEmailCommunication> preferredEmailCommunication;
  String externalSystemId;
  Date enrollmentDate;
  Date createdDate;
  Date updatedDate;

  @Builder.Default
  Map<String, Object> customFields = new LinkedHashMap<>();

  public User withDepartments(Set<String> departments) {
    return User.builder()
      .id(this.id)
      .username(this.username)
      .barcode(barcode)
      .active(active)
      .type(type)
      .expirationDate(expirationDate)
      .personal(personal)
      .patronGroup(patronGroup)
      .tags(tags)
      .metadata(metadata)
      .departments(departments)
      .preferredEmailCommunication(preferredEmailCommunication)
      .externalSystemId(externalSystemId)
      .enrollmentDate(enrollmentDate)
      .customFields(customFields)
      .build();
  }
}
