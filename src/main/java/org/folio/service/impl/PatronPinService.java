package org.folio.service.impl;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PatronPinService {
  private static final Logger logger = LogManager.getLogger(PatronPinService.class);
  public String derivePin(String pin, String userId) {
    try {
      SecretKeyFactory pbkdf2KeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512") ;
      PBEKeySpec keySpec = new PBEKeySpec(pin.toCharArray(), // Input character array of password
                                          userId.getBytes(), // We should add tenant is here also?
                                          150000, // Iteration count (c)
                                          64) ; // 256 bits output hashed password

      // Computes hashed password using PBKDF2HMACSHA512 algorithm and provided PBE specs.
      byte[] pbkdfHashedArray = pbkdf2KeyFactory.generateSecret(keySpec).getEncoded();

      return javax.xml.bind.DatatypeConverter.printHexBinary(pbkdfHashedArray);
    }
    catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
      logger.error("Unable to encode pin", e);
      return null;
    }
  }
}
