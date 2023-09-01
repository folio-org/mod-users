package org.folio.event.util;

import lombok.Getter;

@Getter
public enum UserType {

  PATRON("patron"), STAFF("staff"), SHADOW("shadow");

  private final String typeName;

  UserType(String typeName) {
    this.typeName = typeName;
  }
}
