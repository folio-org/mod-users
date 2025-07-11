package org.folio.support.kafka;

import static io.vertx.kafka.client.consumer.KafkaConsumer.create;
import static java.util.Collections.emptyList;
import static org.apache.kafka.clients.consumer.ConsumerConfig.AUTO_OFFSET_RESET_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.GROUP_ID_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG;
import static org.apache.kafka.clients.consumer.ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG;
import static org.folio.extensions.KafkaContainerExtension.getBootstrapServers;
import static org.folio.support.TestConstants.ENV;
import static org.folio.support.TestConstants.TENANT_NAME;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumer;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.serialization.JsonObjectDeserializer;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class FakeKafkaConsumer {

  private static final Logger logger = LogManager.getLogger(FakeKafkaConsumer.class);
  private static final String USER_GROUP_TOPIC_NAME = String.format("%s.%s.users.userGroup", ENV, TENANT_NAME);
  private static final String USERS_TOPIC_NAME = String.format("%s.%s.users.users", ENV, TENANT_NAME);

  private final Map<String, Map<String, List<KafkaConsumerRecord<String, JsonObject>>>> topicToEvents =
    new ConcurrentHashMap<>();

  private KafkaConsumer<String, JsonObject> consumer;

  public FakeKafkaConsumer consume(Vertx vertx, String... topicNames) {
    var topicNamesSet = getOrDefaultTopicNames(topicNames);
    consumer = create(vertx, consumerProperties());

    consumer.subscribe(topicNamesSet);

    consumer.handler(message -> {
      logger.info("Message received: topic={}, key={}, value={}",
        message.topic(), message.key(), message.value());
      var recordEvents = topicToEvents.computeIfAbsent(message.topic(), v -> new ConcurrentHashMap<>());
      var storageList = recordEvents.computeIfAbsent(message.key(), k -> new CopyOnWriteArrayList<>());
      storageList.add(message);
    });

    return this;
  }

  public Future<Void> closeAsync() {
    if (consumer != null) {
      Future<Void> result = consumer.close();
      consumer = null;
      return result;
    }
    return Future.succeededFuture();
  }

  public void removeAllEvents() {
    topicToEvents.clear();
  }

  public Collection<KafkaConsumerRecord<String, JsonObject>> getUserGroupsEvents(
    String userGroupId) {

    return getEvents(USER_GROUP_TOPIC_NAME, userGroupId);
  }

  public Collection<KafkaConsumerRecord<String, JsonObject>> getUsersEvents(
    String userId) {

    return getEvents(USERS_TOPIC_NAME, userId);
  }

  public Collection<KafkaConsumerRecord<String, JsonObject>> getEvents(
    String topicName, String messageKey) {

    return topicToEvents
      .getOrDefault(topicName, Collections.emptyMap())
      .getOrDefault(messageKey, emptyList());
  }

  public Collection<KafkaConsumerRecord<String, JsonObject>> getAllEvents(String topicName) {
    return topicToEvents
      .getOrDefault(topicName, Collections.emptyMap())
      .values()
      .stream()
      .flatMap(Collection::stream)
      .toList();
  }

  public KafkaConsumerRecord<String, JsonObject> getLastUserGroupEvent(String userGroupId) {
    return getLastEvent(getUserGroupsEvents(userGroupId));
  }

  public KafkaConsumerRecord<String, JsonObject> getLastUserEvent(String userGroupId) {
    return getLastEvent(getUsersEvents(userGroupId));
  }

  private static KafkaConsumerRecord<String, JsonObject> getLastEvent(
    Collection<KafkaConsumerRecord<String, JsonObject>> events) {

    return events.stream().skip(events.size() - 1).findFirst().orElse(null);
  }

  private static Set<String> getOrDefaultTopicNames(String[] topicNames) {
    return ArrayUtils.isEmpty(topicNames)
      ? Set.of(USER_GROUP_TOPIC_NAME, USERS_TOPIC_NAME)
      : Set.of(topicNames);
  }

  private static Map<String, String> consumerProperties() {
    Map<String, String> config = new HashMap<>();

    config.put(BOOTSTRAP_SERVERS_CONFIG, getBootstrapServers());
    config.put(KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    config.put(VALUE_DESERIALIZER_CLASS_CONFIG, JsonObjectDeserializer.class.getName());
    config.put(GROUP_ID_CONFIG, "folio_test");
    config.put(AUTO_OFFSET_RESET_CONFIG, "latest");
    config.put(ENABLE_AUTO_COMMIT_CONFIG, "true");

    return config;
  }
}
