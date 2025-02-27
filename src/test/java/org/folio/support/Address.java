package org.folio.support;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Address {
  String addressTypeId;
  String countryId;
  String addressLine1;
  String addressLine2;
  String city;
  String region;
  String postalCode;
  Boolean primaryAddress;
}
