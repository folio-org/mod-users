package org.folio.rest.impl;

import java.util.HashMap;
import java.util.Map;

import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.User;

class AddressValidator {
  boolean hasMultipleAddressesWithSameType(User user) {
    Map<String, Integer> countMap = new HashMap<>();
    if (user.getPersonal() != null &&
      user.getPersonal().getAddresses() != null) {
      for (Address address : user.getPersonal().getAddresses()) {
        String addressTypeId = address.getAddressTypeId();
        if (addressTypeId != null) {
          boolean found = false;
          for (String key : countMap.keySet()) {
            if (key.equals(addressTypeId)) {
              Integer count = countMap.get(key);
              count = count + 1;
              countMap.put(key, count);
              found = true;
              break;
            }
          }
          if (!found) {
            countMap.put(addressTypeId, 1);
          }
        }
      }
    }
    for (Integer i : countMap.values()) {
      if (i > 1) {
        return true;
      }
    }
    return false;
  }
}
