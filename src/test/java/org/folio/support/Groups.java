package org.folio.support;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
@Builder
@Jacksonized
@JsonIgnoreProperties(ignoreUnknown = true)
public class Groups {
  List<Group> usergroups;
  int totalRecords;

  public Group getGroupByName(String name) {
    return usergroups.stream()
      .filter(group -> Objects.equals(group.getGroup(), name))
      .findFirst().orElse(null);
  }

  public List<String> getNames() {
    return usergroups.stream()
      .map(Group::getGroup)
      .collect(Collectors.toList());
  }
}
