package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.USER_CREATED;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
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
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
@Timeout(value = 10, unit = SECONDS)
public class UserOutboxProcessIT extends AbstractRestTestNoData {

  private static final OutboxEventLog OUTBOX_EVENT_LOG = new OutboxEventLog()
    .withEventId(UUID.randomUUID().toString())
    .withAction(UserEvent.Action.CREATE.value())
    .withActionDate(new Date())
    .withEntityType(OutboxEventLog.EntityType.USER)
    .withPayload(Json.encode(new User().withMetadata(new Metadata())))
    .withIsPersonalDataChanged(true);

  private static TimerInterfaceClient timerInterfaceClient;
  private static UserEventsLogRepository userEventsLogRepository;

  @BeforeAll
  public static void beforeAll() {
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
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
    List<String> list = checkKafkaEventSent(TENANT_NAME, USER_CREATED.getTopicName());
    assertEquals(1, list.size());
    JsonObject log = (JsonObject) Json.decodeValue(list.get(0));
    assertEquals(UserEvent.Action.CREATE.value(), log.getString("action"));
    assertTrue(log.getBoolean("isPersonalDataChanged"));
  }

  @Test
  void shouldFailAfterTriggerUsingNonExistsTenant() {
    timerInterfaceClient.attemptToTriggerUsersOutboxProcess("non-exist")
      .statusCode(is(HTTP_INTERNAL_ERROR));
    List<String> list = checkKafkaEventSent(TENANT_NAME, USER_CREATED.getTopicName());
    assertEquals(0, list.size());
  }

}
