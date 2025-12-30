package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.support.tags.UnitTest;

import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

@UnitTest
class MultipleAddressesWithSameTypeValidationTests {
  private static final String HOME_ADDRESS_TYPE = UUID.randomUUID().toString();
  private static final String CLAIM_ADDRESS_TYPE = UUID.randomUUID().toString();
  private final AddressValidator validator = new AddressValidator();

  @Test
  void validWhenUserHasNoPersonalInformation() {
    final var user = new User();

    user.setPersonal(null);

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void validWhenUserHasNullAddresses() {
    final var user = userWithAddresses(null);

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void validWhenUserHasNoAddresses() {
    final var user = userWithAddresses(Collections.emptyList());

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void validWhenUserHasAddressesOfDifferentTypes() {
    final var user = userWithAddresses(List.of(address(HOME_ADDRESS_TYPE),
      address(CLAIM_ADDRESS_TYPE)));

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void invalidWhenUserHasTwoAddressesOfSameType() {
    final User user = userWithAddresses(List.of(address(HOME_ADDRESS_TYPE),
      address(HOME_ADDRESS_TYPE)));

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(true));
  }

  @NotNull
  private User userWithAddresses(List<Address> addresses) {
    final var user = new User();

    final var personalInformation = new Personal();

    personalInformation.setLastName("sanderson");

    personalInformation.setAddresses(addresses);

    user.setPersonal(personalInformation);

    return user;
  }

  @NotNull
  private Address address(String addressType) {
    final var firstAddress = new Address();

    firstAddress.setAddressTypeId(addressType);

    return firstAddress;
  }
}
