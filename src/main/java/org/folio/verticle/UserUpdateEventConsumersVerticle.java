package org.folio.verticle;

import static org.folio.event.ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_UPDATED;
import static org.folio.event.ConsortiumEventType.USER_UPDATED;

import java.util.List;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.verticle.consumers.ConsortiumUpdateEventsHandler;
import org.folio.verticle.consumers.UserUpdateEventsHandler;

public class UserUpdateEventConsumersVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(USER_UPDATED.getTopicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new UserUpdateEventsHandler(vertx);
  }
}
