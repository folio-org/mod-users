package org.folio.support;

public class ProfilePictureHelper {

  private ProfilePictureHelper() {}

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
