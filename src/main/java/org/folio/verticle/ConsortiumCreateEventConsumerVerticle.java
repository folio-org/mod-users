package org.folio.verticle;

import org.folio.kafka.AsyncRecordHandler;
import org.folio.verticle.consumers.ConsortiumCreateEventHandler;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.folio.event.ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED;

public class ConsortiumCreateEventConsumerVerticle extends AbstractConsumersVerticle {

  public List<String> getEvents() {
    return List.of(CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName());
  }

  public AsyncRecordHandler<String, String> getHandler() {
    return new ConsortiumCreateEventHandler(vertx);
  }
}
