package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;
import java.util.UUID;

import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.junit.jupiter.api.Test;

class MultipleAddressesWithSameTypeValidationTests {
  private final String HOME_ADDRESS_TYPE = UUID.randomUUID().toString();
  private final String CLAIM_ADDRESS_TYPE = UUID.randomUUID().toString();

  private final User user = new User();
  private final AddressValidator validator = new AddressValidator();

  @Test
  void validWhenUserHasAddressesOfDifferentTypes() {
    final var personal = new Personal();

    personal.setLastName("sanderson");

    final var firstAddress = new Address();

    firstAddress.setAddressTypeId(HOME_ADDRESS_TYPE);

    final var secondAddress = new Address();

    secondAddress.setAddressTypeId(CLAIM_ADDRESS_TYPE);

    personal.setAddresses(List.of(firstAddress, secondAddress));

    user.setPersonal(personal);

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void invalidWhenUserHasTwoAddressesOfSameType() {
    final var personal = new Personal();

    personal.setLastName("sanderson");

    final var firstAddress = new Address();

    firstAddress.setAddressTypeId(HOME_ADDRESS_TYPE);

    final var secondAddress = new Address();

    secondAddress.setAddressTypeId(HOME_ADDRESS_TYPE);

    personal.setAddresses(List.of(firstAddress, secondAddress));

    user.setPersonal(personal);

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(true));
  }

  @Test
  void validWhenUserHasNoAddresses() {
    final var personal = new Personal();

    personal.setLastName("sanderson");

    user.setPersonal(personal);

    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }

  @Test
  void validWhenUserHasNoPersonalInformation() {
    assertThat(validator.hasMultipleAddressesWithSameType(user), is(false));
  }
}
