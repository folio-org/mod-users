package org.folio.support;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
public class Group {
  String group;
  String desc;
  Integer expirationOffsetInDays;
}
