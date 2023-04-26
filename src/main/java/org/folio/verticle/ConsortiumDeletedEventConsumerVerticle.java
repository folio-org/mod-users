package org.folio.verticle;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.verticle.consumers.ConsortiumDeleteEventHandler;

import java.util.List;

import static org.folio.event.ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_DELETED;

public class ConsortiumDeletedEventConsumerVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(CONSORTIUM_PRIMARY_AFFILIATION_DELETED.getTopicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new ConsortiumDeleteEventHandler(vertx);
  }
}
