package org.folio.event.util;

public enum PomReaderUtil {
  INSTANCE;

  public String constructModuleVersionAndVersion(String moduleName, String moduleVersion) {
    String result = moduleName.replace("_", "-");
    return result + "-" + moduleVersion;
  }
}
