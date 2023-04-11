package org.folio.event;

public enum UserEventType {
  USER_CREATED("USER_CREATED");

  private final String topicName;

  UserEventType(String topicName) {
    this.topicName = topicName;
  }

  public String getTopicName() {
    return topicName;
  }
}
