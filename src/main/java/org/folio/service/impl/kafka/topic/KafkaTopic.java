package org.folio.service.impl.kafka.topic;

import java.util.stream.Stream;

public enum KafkaTopic {
  USER_INSTANCE("user.instance");
  //INVENTORY_ITEM("inventory.item");

  private final String topicName;

  KafkaTopic(String topicName) {
    this.topicName = topicName;
  }

  public String getTopicName() {
    return topicName;
  }

  public static KafkaTopic forName(String name) {
    return Stream.of(values())
      .filter(value -> value.getTopicName().equals(name))
      .findFirst()
      .orElse(null);
  }
}
