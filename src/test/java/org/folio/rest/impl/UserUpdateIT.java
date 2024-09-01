package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.service.event.DomainEventType.UPDATED;
import static org.folio.support.kafka.topic.UsersKafkaTopic.USERS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.UUID;

import org.folio.event.service.UserUpdateService;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.service.event.DomainEvent;
import org.folio.service.event.EntityChangedData;
import org.folio.support.User;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;

@ExtendWith(VertxExtension.class)
public class UserUpdateIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static UserUpdateService userUpdateService;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    userUpdateService = new UserUpdateService();
  }

  @Test
  void sendUserUpdatedEvent() {
    var id = UUID.randomUUID().toString();
    var barcode = "123456";
    var username = "julia";
    usersClient.createUser(User.builder()
      .id(id)
      .username(username)
      .barcode(barcode)
      .build());

    var user = new org.folio.rest.jaxrs.model.User()
      .withId(id)
      .withBarcode(barcode)
      .withUsername(username);
    var userJson = new JsonObject(Json.encode(user));
    user.setBarcode("654321");
    var updatedUserJson = new JsonObject(Json.encode(user));
    EntityChangedData<JsonObject> data = new EntityChangedData<>(userJson, updatedUserJson);

    DomainEvent event = DomainEvent.builder()
      .id(UUID.randomUUID())
      .type(UPDATED)
      .tenant("diku")
      .timestamp(System.currentTimeMillis())
      .data(data)
      .build();

    sendEvent(TENANT_NAME, USERS.topicName(), event.getTenant(), Json.encode(event));
    awaitHandlingEvent(id, user.getBarcode());
  }

  @Test
  void cannotUpdateIfBarcodeAndPatronGroupNotChanged() {
    var id = UUID.randomUUID().toString();
    var barcode = "123456";
    var username = "julia";

    usersClient.createUser(User.builder()
      .id(id)
      .username(username)
      .barcode(barcode)
      .build());

    var user = new org.folio.rest.jaxrs.model.User()
      .withId(id)
      .withBarcode(barcode)
      .withPatronGroup(UUID.randomUUID().toString())
      .withUsername("julia");

    userUpdateService.updateUser(user, user.withUsername("Julia S"), TENANT_NAME, vertx, null);
    final var updatedUser = usersClient.getUser(id);
    assertThat(updatedUser.getUsername(), is(username));
  }

  private void awaitHandlingEvent(String userId, String barcode) {
    Awaitility.await()
      .atMost(1, MINUTES)
      .pollInterval(5, SECONDS)
      .untilAsserted(() -> {
        final var updatedUser = usersClient.getUser(userId);
        assertThat(updatedUser.getBarcode(), is(barcode));
      });
  }
}
