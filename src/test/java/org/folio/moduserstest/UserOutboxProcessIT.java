package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.USER_CREATED;
import static org.folio.extensions.KafkaContainerExtension.getTopicName;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.folio.support.matchers.DomainEventAssertions.assertHeaders;
import static org.folio.support.matchers.DomainEventAssertions.await;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.Date;
import java.util.UUID;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import org.folio.repository.UserEventsLogRepository;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.OutboxEventLog;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.persist.PostgresClient;
import org.folio.support.http.TimerInterfaceClient;
import org.folio.support.kafka.FakeKafkaConsumer;
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
@Timeout(value = 10, unit = SECONDS)
public class UserOutboxProcessIT extends AbstractRestTestNoData {

  private static final String USER_ID = UUID.randomUUID().toString();

  private static final OutboxEventLog OUTBOX_EVENT_LOG = new OutboxEventLog()
    .withEventId(UUID.randomUUID().toString())
    .withAction(UserEvent.Action.CREATE.value())
    .withActionDate(new Date())
    .withEntityType(OutboxEventLog.EntityType.USER)
    .withPayload(Json.encode(new User().withId(USER_ID).withMetadata(new Metadata())))
    .withIsPersonalDataChanged(true);

  private static final String expectedTopic =
    getTopicName(TENANT_NAME, USER_CREATED.getTopicName());

  private static TimerInterfaceClient timerInterfaceClient;
  private static UserEventsLogRepository userEventsLogRepository;
  private static FakeKafkaConsumer kafkaConsumer;

  @BeforeAll
  public static void beforeAll() {
    kafkaConsumer = new FakeKafkaConsumer().consume(module.getVertx(), expectedTopic);
    userEventsLogRepository = new UserEventsLogRepository();
    timerInterfaceClient = new TimerInterfaceClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach(Vertx vertx, VertxTestContext context) {
    PostgresClient postgresClient = PostgresClient.getInstance(vertx, TENANT_NAME);
    postgresClient.withConn(conn -> userEventsLogRepository.saveEventLog(conn, OUTBOX_EVENT_LOG, TENANT_NAME))
      .onComplete(context.succeedingThenComplete());
    kafkaConsumer.removeAllEvents();
  }

  @AfterAll
  static void afterAll(VertxTestContext context) {
    kafkaConsumer.closeAsync().onComplete(context.succeedingThenComplete());
  }

  @Test
  void shouldSendUserEventToKafkaAfterTrigger() {
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
    await().until(() -> kafkaConsumer.getEvents(expectedTopic, USER_ID), hasSize(1));
    var list = kafkaConsumer.getEvents(expectedTopic, USER_ID);

    var message = list.iterator().next();
    var log = message.value();
    assertHeaders(message.headers());
    assertEquals(UserEvent.Action.CREATE.value(), log.getString("action"));
    assertTrue(log.getBoolean("isPersonalDataChanged"));
  }

  @Test
  void shouldFailAfterTriggerUsingNonExistsTenant() {
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess("non-exist")
      .statusCode(is(HTTP_INTERNAL_ERROR));
    await(3).until(() -> kafkaConsumer.getAllEvents(expectedTopic), hasSize(0));
    kafkaConsumer.getAllEvents(expectedTopic);
  }
}
