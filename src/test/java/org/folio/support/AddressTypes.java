package org.folio.support;

import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddressTypes {
  List<AddressType> addressTypes;
  int totalRecords;

  public List<String> getNames() {
    return addressTypes.stream()
      .map(AddressType::getAddressType)
      .collect(Collectors.toList());
  }
}
