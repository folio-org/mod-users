package org.folio.moduserstest;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.repository.UserEventsLogRepository;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.OutboxEventLog;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.persist.PostgresClient;
import org.folio.support.http.TimerInterfaceClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static java.net.HttpURLConnection.*;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.USER_CREATED;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(VertxExtension.class)
@Timeout(value = 10, unit = SECONDS)
public class UserOutboxProcessIT extends AbstractRestTest {

  private static final OutboxEventLog OUTBOX_EVENT_LOG = new OutboxEventLog()
    .withEventId(UUID.randomUUID().toString())
    .withAction(UserEvent.Action.CREATE.value())
    .withActionDate(new Date())
    .withEntityType(OutboxEventLog.EntityType.USER)
    .withPayload(Json.encode(new User().withMetadata(new Metadata())));

  private static TimerInterfaceClient timerInterfaceClient;
  private static UserEventsLogRepository userEventsLogRepository;

  @BeforeAll
  public static void beforeAll() {
    LOAD_SAMPLE_DATA = false;
    LOAD_REFERENCE_DATA = false;
    userEventsLogRepository = new UserEventsLogRepository();
    timerInterfaceClient = new TimerInterfaceClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach(Vertx vertx, VertxTestContext context) {
    PostgresClient postgresClient = PostgresClient.getInstance(vertx, TENANT_NAME);
    postgresClient.withConn(conn -> userEventsLogRepository.saveEventLog(conn, OUTBOX_EVENT_LOG, TENANT_NAME))
      .onComplete(context.succeedingThenComplete());
  }

  @Test
  void shouldSendUserEventToKafkaAfterTrigger() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
    List<String> list = checkKafkaEventSent(TENANT_NAME, USER_CREATED.getTopicName());
    assertEquals(1, list.size());
  }

  @Test
  void shouldFailAfterTriggerUsingNonExistsTenant() {
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess("non-exist")
      .statusCode(is(HTTP_INTERNAL_ERROR));
    List<String> list = checkKafkaEventSent(TENANT_NAME, USER_CREATED.getTopicName());
    assertEquals(0, list.size());
  }

}
