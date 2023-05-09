package org.folio.verticle;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.verticle.consumers.ConsortiumCreateEventsHandler;

import java.util.List;

import static org.folio.event.ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED;

public class ConsortiumCreateEventConsumersVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new ConsortiumCreateEventsHandler(vertx);
  }
}
