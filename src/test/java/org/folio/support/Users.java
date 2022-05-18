package org.folio.support;

import java.util.List;
import java.util.Objects;

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
  ResultInfo resultInfo;
  int totalRecords;

  public User getFirstUser() {
    return users.get(0);
  }

  public Integer getFacetCount(String value) {
    // This assumes only the first facet is of interest
    return resultInfo.getFacets().get(0)
      .getFacetValues().stream()
      .filter(facetValue -> Objects.equals(facetValue.getValue(), value))
      .map(FacetValue::getCount)
      .findFirst().orElse(null);
  }
}
