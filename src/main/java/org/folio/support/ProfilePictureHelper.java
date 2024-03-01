package org.folio.support;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidParameterSpecException;

public class ProfilePictureHelper {

  private ProfilePictureHelper() {}

  @SuppressWarnings("java:S5542")
  public static byte[] encryptAES(byte[] input, String key) throws NoSuchAlgorithmException, NoSuchPaddingException,
    InvalidKeyException, BadPaddingException, IllegalBlockSizeException, InvalidParameterSpecException {
    Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
    SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "AES");
    cipher.init(Cipher.ENCRYPT_MODE, secretKey);
    AlgorithmParameters params = cipher.getParameters();
    byte[] iv = params.getParameterSpec(IvParameterSpec.class).getIV();
    byte[] encryptedData = cipher.doFinal(input);

    byte[] result = new byte[iv.length + encryptedData.length];
    System.arraycopy(iv, 0, result, 0, iv.length);
    System.arraycopy(encryptedData, 0, result, iv.length, encryptedData.length);

    return result;
  }

  @SuppressWarnings("java:S5542")
  public static byte[] decryptAES(byte[] input, String key) throws NoSuchAlgorithmException, NoSuchPaddingException,
    InvalidKeyException, BadPaddingException, IllegalBlockSizeException, InvalidAlgorithmParameterException {
    Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
    SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "AES");
    byte[] iv = new byte[16]; // Assuming 16 bytes IV length
    System.arraycopy(input, 0, iv, 0, 16);
    IvParameterSpec ivParameterSpec = new IvParameterSpec(iv);
    cipher.init(Cipher.DECRYPT_MODE, secretKey, ivParameterSpec);
    return cipher.doFinal(input, 16, input.length - 16);
  }

  public static byte[] calculateHmac(byte[] data, String key) throws NoSuchAlgorithmException, InvalidKeyException {
    Mac mac = Mac.getInstance("HmacSHA256");
    SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "HmacSHA256");
    mac.init(secretKey);
    return mac.doFinal(data);
  }

  public static boolean verifyHmac(byte[] data, byte[] storedHmac, String key) throws NoSuchAlgorithmException, InvalidKeyException {
    byte[] calculatedHmac = calculateHmac(data, key);
    return MessageDigest.isEqual(calculatedHmac, storedHmac);
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

  public static double bytesToMegabytes(long bytes) {
    return (double) bytes / (1024 * 1024);
  }
}
