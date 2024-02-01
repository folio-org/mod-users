package org.folio.support;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;

public class ProfilePictureHelper {

  private ProfilePictureHelper() {}

  private static final String SECRET_KEY_ALGORITHM = "AES";
  private static final String CIPHER_ALGORITHM = "AES/CBC/PKCS5Padding";

  @SuppressWarnings("java:S112")
  public static byte[] encrypt(byte[] input, String encryptionKey) throws Exception {
    SecretKey secretKey = generateSecretKey(encryptionKey);
    Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);

    byte[] ivBytes = new byte[cipher.getBlockSize()];
    SecureRandom secureRandom = new SecureRandom();
    secureRandom.nextBytes(ivBytes);
    IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);

    cipher.init(Cipher.ENCRYPT_MODE, secretKey, ivParameterSpec);
    byte[] encryptedBytes = cipher.doFinal(input);

    byte[] combined = new byte[ivBytes.length + encryptedBytes.length];
    System.arraycopy(ivBytes, 0, combined, 0, ivBytes.length);
    System.arraycopy(encryptedBytes, 0, combined, ivBytes.length, encryptedBytes.length);

    return combined;
  }

  @SuppressWarnings("java:S112")
  public static byte[] decrypt(byte[] combined, String encryptionKey) throws Exception {
    SecretKey secretKey = generateSecretKey(encryptionKey);
    Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);

    byte[] ivBytes = Arrays.copyOfRange(combined, 0, cipher.getBlockSize());
    IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);

    cipher.init(Cipher.DECRYPT_MODE, secretKey, ivParameterSpec);
    return cipher.doFinal(Arrays.copyOfRange(combined, ivBytes.length, combined.length));
  }

  private static SecretKey generateSecretKey(String encryptionKey) throws NoSuchAlgorithmException {
    byte[] keyBytes = deriveKeyBytes(encryptionKey);
    return new SecretKeySpec(keyBytes, SECRET_KEY_ALGORITHM);
  }

  private static byte[] deriveKeyBytes(String encryptionKey) throws NoSuchAlgorithmException {
    MessageDigest sha = MessageDigest.getInstance("SHA-256");
    return sha.digest(encryptionKey.getBytes());
  }
  public static String detectFileType(byte[] data) {
    if (data.length < 12) {
      throw new IllegalArgumentException("Insufficient data provided to detect file type");
    }
    if (isJPEG(data)) {
      return "JPEG";
    } else if (isPNG(data)) {
      return "PNG";
    }
    return "Unknown";
  }

  // Helper methods to check file signatures for JPEG and PNG
  private static boolean isJPEG(byte[] data) {
    return data[0] == (byte) 0xFF && data[1] == (byte) 0xD8 && data[2] == (byte) 0xFF && data[3] == (byte) 0xE0;
  }

  private static boolean isPNG(byte[] data) {
    return data[0] == (byte) 0x89 && data[1] == 'P' && data[2] == 'N' && data[3] == 'G';
  }
}
