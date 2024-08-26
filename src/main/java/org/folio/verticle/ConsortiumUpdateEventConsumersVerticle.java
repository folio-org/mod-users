package org.folio.verticle;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.support.kafka.topic.UsersKafkaTopic;
import org.folio.verticle.consumers.ConsortiumUpdateEventsHandler;

import java.util.List;

import static org.folio.event.ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_UPDATED;

public class ConsortiumUpdateEventConsumersVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(CONSORTIUM_PRIMARY_AFFILIATION_UPDATED.getTopicName(),
      UsersKafkaTopic.USER_UPDATED.topicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new ConsortiumUpdateEventsHandler(vertx);
  }
}
