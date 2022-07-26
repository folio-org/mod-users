package org.folio.support;

import java.time.ZonedDateTime;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Personal {
  String lastName;
  String firstName;
  String preferredFirstName;
  ZonedDateTime dateOfBirth;
  List<Address> addresses;
}
