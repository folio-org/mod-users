package org.folio.support.matchers;

import static io.vertx.core.MultiMap.caseInsensitiveMultiMap;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.kafka.KafkaHeaderUtils.kafkaHeadersToMap;
import static org.folio.moduserstest.AbstractRestTest.TENANT_NAME;
import static org.folio.okapi.common.XOkapiHeaders.TENANT;
import static org.folio.okapi.common.XOkapiHeaders.USER_ID;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;
import java.util.UUID;

import org.awaitility.Awaitility;
import org.awaitility.core.ConditionFactory;
import org.folio.service.event.DomainEventType;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeMatcher;

import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import io.vertx.kafka.client.producer.KafkaHeader;
import lombok.SneakyThrows;

public final class DomainEventAssertions {

  private static final String FOLIO_TENANT_ID = "folio.tenantId";

  private DomainEventAssertions() { }

  public static ConditionFactory await() {
    return Awaitility.await().atMost(5, SECONDS);
  }


  public static void assertBasicEventFields(KafkaConsumerRecord<String, JsonObject> event,
      DomainEventType expectedType) {
    JsonObject value = event.value();

    assertThat(value.getString("id"), hasUUIDFormat());
    assertThat(value.getString("type"), is(expectedType.name()));
    assertThat(value.getString("tenant"), is(TENANT_NAME));
    assertThat(value.getLong("timestamp"), is(notNullValue()));
  }

  @SneakyThrows
  public static void assertHeaders(List<KafkaHeader> headers) {
    final MultiMap caseInsensitiveMap = caseInsensitiveMultiMap()
        .addAll(kafkaHeadersToMap(headers));

    assertEquals(4, caseInsensitiveMap.size());
    assertEquals(TENANT_NAME, caseInsensitiveMap.get(TENANT));
    assertEquals(TENANT_NAME, caseInsensitiveMap.get(FOLIO_TENANT_ID));
    assertNotNull(USER_ID);
  }

  private static JsonObject getDataValue(KafkaConsumerRecord<String, JsonObject> event, String field) {
    return event.value().getJsonObject("data", new JsonObject()).getJsonObject(field);
  }

  private static Matcher<String> hasUUIDFormat() {

    return new TypeSafeMatcher<String>() {

      @Override
      protected boolean matchesSafely(String uuidAsString) {
        try {
          UUID.fromString(uuidAsString);
        } catch (IllegalArgumentException ex) {
          return false;
        }

        return true;
      }

      @Override
      public void describeTo(Description description) {
        description.appendText("should be an UUID string");
      }

    };
  }
}
