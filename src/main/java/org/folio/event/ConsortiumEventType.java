package org.folio.event;

public enum ConsortiumEventType {
  CONSORTIUM_PRIMARY_AFFILIATION_CREATED("CONSORTIUM_PRIMARY_AFFILIATION_CREATED"),
  CONSORTIUM_PRIMARY_AFFILIATION_DELETED("CONSORTIUM_PRIMARY_AFFILIATION_DELETED");
  private final String topicName;

  ConsortiumEventType(String topicName) {
    this.topicName = topicName;
  }

  public String getTopicName() {
    return topicName;
  }
}
