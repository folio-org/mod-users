package org.folio.support;

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
  String pronouns;
  String lastName;
  String firstName;
  String middleName;
  String preferredFirstName;
  String dateOfBirth;
  List<Address> addresses;
  String email;
  String phone;
  String mobilePhone;
  String preferredContactTypeId;
  String profilePictureLink;
}
