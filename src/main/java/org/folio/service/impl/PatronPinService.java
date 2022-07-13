package org.folio.service.impl;

import static javax.xml.bind.DatatypeConverter.printHexBinary;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PatronPinService {
  private static final Logger logger = LogManager.getLogger(PatronPinService.class);
  public String derivePin(String pin, String userId) {
    try {
      // Should the salt also include the tenant?
      SecretKey derivedPin = hashPassword(pin, userId);

      return printHexBinary(derivedPin.getEncoded());
    }
    catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
      logger.error("Unable to encode pin", e);
      return null;
    }
  }

  private SecretKey hashPassword(String password, String salt)
    throws NoSuchAlgorithmException, InvalidKeySpecException {

    final var ITERATION_COUNT = 150000;
    final var KEY_LENGTH = 64; // 256 bits

    final var keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512");

    final var keySpec = new PBEKeySpec(password.toCharArray(),
      salt.getBytes(), ITERATION_COUNT, KEY_LENGTH);

    return keyFactory.generateSecret(keySpec);
  }
}
