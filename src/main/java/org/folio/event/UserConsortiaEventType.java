package org.folio.event;

public enum UserConsortiaEventType {
  USER_CREATED("USER_CREATED");

  private final String topicName;

  UserConsortiaEventType(String topicName) {
    this.topicName = topicName;
  }

  public String getTopicName() {
    return topicName;
  }
}
