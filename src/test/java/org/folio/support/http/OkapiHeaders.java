package org.folio.support.http;

import lombok.Value;

@Value
public class OkapiHeaders {
  String okapiUrl;
  String tenantId;
  String token;
}
