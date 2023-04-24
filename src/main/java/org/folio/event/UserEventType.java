package org.folio.event;

public enum UserEventType {
  USER_CREATED("USER_CREATED"),
  USER_DELETED("USER_DELETED");
  private final String topicName;

  UserEventType(String topicName) {
    this.topicName = topicName;
  }

  public String getTopicName() {
    return topicName;
  }
}
