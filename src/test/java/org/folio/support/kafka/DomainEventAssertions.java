package org.folio.support.kafka;

import static org.folio.support.matchers.DomainEventAssertions.assertBasicEventFields;
import static org.folio.support.matchers.DomainEventAssertions.assertHeaders;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import org.folio.service.event.DomainEventType;

import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;

public class DomainEventAssertions {
  public static void assertCreateEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Create event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.CREATED);
    assertHeaders(createEvent.headers());
  }

  public static void assertUpdateEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Update event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.UPDATED);
    assertHeaders(createEvent.headers());
  }

  public static void assertDeleteEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Delete event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.DELETED);
    assertHeaders(createEvent.headers());
  }
}
