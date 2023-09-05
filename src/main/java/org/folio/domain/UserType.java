package org.folio.domain;

public enum UserType {

  PATRON("patron"),
  STAFF("staff"),
  SHADOW("shadow"),
  SYSTEM("system");

  private final String typeName;

  UserType(String typeName) {
    this.typeName = typeName;
  }

  public String getTypeName() {
    return typeName;
  }
}
