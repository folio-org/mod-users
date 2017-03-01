package org.folio.rest.dao;

import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author shale
 *
 */
public class Group2User {

  @NotNull
  @JsonProperty("groupId")
  @JsonInclude(Include.NON_NULL)
  private String groupId;

  @NotNull
  @JsonProperty("userId")
  @JsonInclude(Include.NON_NULL)
  private String userId;

  public String getGroupId() {
    return groupId;
  }
  public void setGroupId(String groupId) {
    this.groupId = groupId;
  }
  public String getUserId() {
    return userId;
  }
  public void setUserId(String userId) {
    this.userId = userId;
  }

}
