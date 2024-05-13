package org.folio.support.kafka.topic;

import org.folio.kafka.services.KafkaTopic;

public enum UsersKafkaTopic implements KafkaTopic {
  USER_GROUP("userGroup", 10);
  private final String topic;
  private final int partitions;

  UsersKafkaTopic(String topic, int partitions) {
    this.topic = topic;
    this.partitions = partitions;
  }

  @Override
  public String moduleName() {
    return "circulation";
  }

  @Override
  public String topicName() {
    return topic;
  }

  @Override
  public int numPartitions() {
    return partitions;
  }
}

