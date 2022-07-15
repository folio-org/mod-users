package org.folio.service.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import lombok.SneakyThrows;

class PatronPinServiceTests {
  @SneakyThrows
  @Test
  void canHandleAlgorithmNotFoundFailureWhenHashingPin() {
    final var passwordHashService = mock(PasswordHashService.class);

    when(passwordHashService.hashPassword(any(), any()))
      .thenThrow(new NoSuchAlgorithmException());

    final var patronPinService = new PatronPinService(passwordHashService);

    assertThat(patronPinService.derivePin("1234", randomUserId()), is(nullValue()));
  }

  @SneakyThrows
  @Test
  void canHandleInvalidKeySpecFailureWhenHashingPin() {
    final var passwordHashService = mock(PasswordHashService.class);

    when(passwordHashService.hashPassword(any(), any()))
      .thenThrow(new InvalidKeySpecException());

    final var patronPinService = new PatronPinService(passwordHashService);

    assertThat(patronPinService.derivePin("1234", randomUserId()), is(nullValue()));
  }

  private String randomUserId() {
    return UUID.randomUUID().toString();
  }
}
