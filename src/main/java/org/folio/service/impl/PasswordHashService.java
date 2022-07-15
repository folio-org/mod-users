package org.folio.service.impl;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

public class PasswordHashService {
  public SecretKey hashPassword(String password, String salt)
    throws NoSuchAlgorithmException, InvalidKeySpecException {

    final var ITERATION_COUNT = 150000;
    final var KEY_LENGTH = 64; // 256 bits

    final var keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");

    final var keySpec = new PBEKeySpec(password.toCharArray(),
      salt.getBytes(), ITERATION_COUNT, KEY_LENGTH);

    return keyFactory.generateSecret(keySpec);
  }
}
