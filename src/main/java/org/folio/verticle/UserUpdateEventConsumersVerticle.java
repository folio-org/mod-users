package org.folio.verticle;

import static org.folio.support.kafka.topic.UsersKafkaTopic.USERS;

import java.util.List;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.verticle.consumers.UserUpdateEventsHandler;

public class UserUpdateEventConsumersVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(USERS.topicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new UserUpdateEventsHandler(vertx);
  }
}
