package org.folio.support.http;

import java.util.Base64;

import org.jetbrains.annotations.NotNull;

public class FakeTokenGenerator {
  @NotNull
  public String generateToken() {
    return String.format("header.%s.signature",
      Base64.getEncoder().encodeToString("{}".getBytes()));
  }
}
