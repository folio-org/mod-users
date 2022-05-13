package org.folio.rest.impl;

import java.util.Map;
import java.util.stream.Collectors;

import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.User;

class AddressValidator {
  boolean hasMultipleAddressesWithSameType(User user) {
    if (user.getPersonal() == null || user.getPersonal().getAddresses() == null) {
      return false;
    }

    return occursMoreThanOnce(byAddressType(user));
  }

  private boolean occursMoreThanOnce(Map<String, Integer> addressTypes) {
    return addressTypes.entrySet().stream()
      .anyMatch(type -> type.getValue() > 1);
  }

  private Map<String, Integer> byAddressType(User user) {
    return user.getPersonal().getAddresses()
      .stream()
      .collect(Collectors.toMap(Address::getAddressTypeId, addressType -> 1,
        Integer::sum));
  }
}
