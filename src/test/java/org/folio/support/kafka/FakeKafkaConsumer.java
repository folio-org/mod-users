package org.folio.support.kafka;

import static io.vertx.kafka.client.consumer.KafkaConsumer.create;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.kafka.common.serialization.StringDeserializer;
import org.folio.kafka.services.KafkaEnvironmentProperties;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumer;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.serialization.JsonObjectDeserializer;

public final class FakeKafkaConsumer {
  private static final String USER_GROUP_TOPIC_NAME = "folio.diku.users.userGroup";
  private static final Map<String, List<KafkaConsumerRecord<String, JsonObject>>> userGroupEvents =
      new ConcurrentHashMap<>();

  private static final Map<String, Map<String, List<KafkaConsumerRecord<String, JsonObject>>>> topicToEvents = Map.of(
    USER_GROUP_TOPIC_NAME, userGroupEvents);

  public FakeKafkaConsumer consume(Vertx vertx) {
    final KafkaConsumer<String, JsonObject> consumer = create(vertx, consumerProperties());

    consumer.subscribe(Set.of(USER_GROUP_TOPIC_NAME));

    consumer.handler(message -> {
      var recordEvents = topicToEvents.get(message.topic());

      if (recordEvents == null) {
        throw new IllegalArgumentException("Undefined topic: " + message.topic());
      }

      var storageList = recordEvents.computeIfAbsent(message.key(), k -> new ArrayList<>());
      storageList.add(message);
    });

    return this;
  }

  public static void removeAllEvents() {
    userGroupEvents.clear();
  }

  public static Collection<KafkaConsumerRecord<String, JsonObject> > getUserGroupsEvents(
    String userGroupId) {

    return userGroupEvents.getOrDefault(userGroupId, emptyList());
  }

  public static KafkaConsumerRecord<String, JsonObject> getLastUserGroupEvent(String userGroupId) {
    return getLastEvent(getUserGroupsEvents(userGroupId));
  }

  private static KafkaConsumerRecord<String, JsonObject> getLastEvent(
    Collection<KafkaConsumerRecord<String, JsonObject>> events) {

    return events.stream().skip(events.size() - 1).findFirst().orElse(null);
  }

  private Map<String, String> consumerProperties() {
    Map<String, String> config = new HashMap<>();
    config.put("bootstrap.servers", KafkaEnvironmentProperties.host() + ":" + KafkaEnvironmentProperties.port());
    config.put("key.deserializer", StringDeserializer.class.getName());
    config.put("value.deserializer", JsonObjectDeserializer.class.getName());
    config.put("group.id", "folio_test");
    config.put("auto.offset.reset", "earliest");
    config.put("enable.auto.commit", "true");

    return config;
  }
}
