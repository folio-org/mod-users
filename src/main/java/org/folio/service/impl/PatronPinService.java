package org.folio.service.impl;

import static javax.xml.bind.DatatypeConverter.printHexBinary;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import javax.crypto.SecretKey;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PatronPinService {
  private static final Logger logger = LogManager.getLogger(PatronPinService.class);

  private final PasswordHashService passwordHashService;

  public PatronPinService(PasswordHashService passwordHashService) {
    this.passwordHashService = passwordHashService;
  }

  public String derivePin(String pin, String userId) {
    try {
      // Should the salt also include the tenant?
      SecretKey derivedPin = passwordHashService.hashPassword(pin, userId);

      return printHexBinary(derivedPin.getEncoded());
    }
    catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
      logger.error("Unable to encode pin", e);
      return null;
    }
  }
}
