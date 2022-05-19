package org.folio.support;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Users {
  List<User> users;
  int totalRecords;

  public User getFirstUser() {
    return getUsers().get(0);
  }
}
