package org.folio.support;

import java.util.List;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
public class Personal {
  String lastName;
  List<Address> addresses;
}
