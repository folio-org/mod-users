package org.folio.rest.impl;

import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.folio.event.ConsortiumEventType;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.support.http.UserTenantClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;

@ExtendWith(VertxExtension.class)
class UserTenantIT extends AbstractRestTestNoData {

  private static UserTenantClient userTenantClient;

  private static final String USER_A = "user_a";
  private static final String USER_B = "user_b";
  private static final String TENANT_X = "tenant_x";
  private static final String TENANT_Y = "tenant_y";

  private static final UserTenant FIRST_AFFILIATION = new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUsername(USER_A).withTenantId(TENANT_X)
    .withConsortiumId(UUID.randomUUID().toString());
  private static final UserTenant SECOND_AFFILIATION = new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUsername(USER_B).withTenantId(TENANT_X);
  private static final UserTenant THIRD_AFFILIATION = new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUsername(USER_A).withTenantId(TENANT_Y);
  private static final UserTenant FOURTH_AFFILIATION = new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUsername(USER_B).withTenantId(TENANT_Y);

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    sendAffiliationCreatedEvent();
    awaitHandlingEvent(3);
  }

  @Test
  void canRetrieveAllUserTenants() {
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(3, collection.getTotalRecords());
  }

  @Test
  void canDeleteAllUserTenants() {
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(3, collection.getTotalRecords());

    sendAffiliationDeletedEvents(collection.getUserTenants());
    UserTenantCollection collection2 = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(0, collection2.getTotalRecords());
  }

  @Test
  void canUpdateUserTenant() {
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();
    UserTenant userTenant = collection.getUserTenants().get(0);
    userTenant.setEmail("Test");
    userTenant.setPhoneNumber("1234");
    userTenant.setUsername("testUser");
    userTenant.setMobilePhoneNumber("000000");
    userTenant.setBarcode("12345");
    userTenant.setExternalSystemId("54321");
    sendAffiliationUpdatedEvent(List.of(userTenant));
    Awaitility.await()
      .atMost(2, MINUTES)
      .pollInterval(5, SECONDS)
      .untilAsserted(() -> {
        UserTenantCollection collection2 = userTenantClient.getAllUsersTenants();
        UserTenant tenant = collection2.getUserTenants().get(2);
        Assertions.assertEquals("Test", tenant.getEmail());
        Assertions.assertEquals("1234", tenant.getPhoneNumber());
        Assertions.assertEquals("testUser", tenant.getUsername());
        Assertions.assertEquals("000000", tenant.getMobilePhoneNumber());
        Assertions.assertEquals("12345", tenant.getBarcode());
        Assertions.assertEquals("54321", tenant.getExternalSystemId());
        Assertions.assertNotNull(tenant.getConsortiumId());
      });
  }

  @Test
  void canSearchByUserId() {
    String userId = SECOND_AFFILIATION.getUserId();
    Map<String, String> params = Map.of("userId", userId);

    UserTenantCollection collection = userTenantClient.getUserTenants(params);

    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(1, collection.getTotalRecords());
    Assertions.assertEquals(userId, userTenant.getUserId());
  }

  @Test
  void canSearchByUserName() {
    Map<String, String> params = Map.of("username", USER_A);

    UserTenantCollection collection = userTenantClient.getUserTenants(params);

    List<UserTenant> userTenants = collection.getUserTenants();
    Assertions.assertEquals(2, collection.getTotalRecords());
    userTenants.forEach(userTenant -> Assertions.assertEquals(USER_A, userTenant.getUsername()));
  }

  @Test
  void canSearchByUserNameAndTenantId() {
    String username = THIRD_AFFILIATION.getUsername();
    String tenantId = THIRD_AFFILIATION.getTenantId();
    Map<String, String> params = Map.of(
      "username", username,
      "tenantId", tenantId);

    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    Assertions.assertEquals(1, collection.getTotalRecords());
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(username, userTenant.getUsername());
    Assertions.assertEquals(tenantId, userTenant.getTenantId());
  }

  @Test
  void canCreateAUserTenant() {
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(3, collection.getTotalRecords());
    Assertions.assertFalse(collection.getUserTenants().contains(FOURTH_AFFILIATION));

    int actualStatusCode = userTenantClient.attemptToSaveUserTenant(FOURTH_AFFILIATION);
    Assertions.assertEquals(201, actualStatusCode);

    collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(4, collection.getTotalRecords());
    Assertions.assertTrue(collection.getUserTenants().contains(FOURTH_AFFILIATION));
    sendAffiliationDeletedEvents(collection.getUserTenants());
  }

  @Test
  void shouldGet422ForMissingRequiredFields() {
    int actualStatusCode = userTenantClient.attemptToSaveUserTenant(new UserTenant());
    Assertions.assertEquals(422, actualStatusCode);
  }

  @Test
  void shouldGet500WhenTryingToSaveAlreadyExistingRecord() {
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(3, collection.getTotalRecords());
    Assertions.assertTrue(collection.getUserTenants().contains(THIRD_AFFILIATION));

    int actualStatusCode = userTenantClient.attemptToSaveUserTenant(THIRD_AFFILIATION);
    Assertions.assertEquals(500, actualStatusCode);

    collection = userTenantClient.getAllUsersTenants();

    Assertions.assertEquals(3, collection.getTotalRecords());
  }

  @Test
  void canSearchByUserNameAndEmailWithOrOperation() {
    String username = "testCrossTenantUser";
    String tenantId = "testTenant";
    String centralTenantId = "testTenant";
    String email = "test@mail.org";

    UserTenant affiliation = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername(username).withTenantId(tenantId).withEmail(email).withCentralTenantId(centralTenantId);

    int actualStatusCode = userTenantClient.attemptToSaveUserTenant(affiliation);
    Assertions.assertEquals(201, actualStatusCode);

    Map<String, String> params = Map.of(
      "username", username,
      "email", username,
      "queryOp", "or");

    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    Assertions.assertEquals(1, collection.getTotalRecords());
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(username, userTenant.getUsername());
    Assertions.assertEquals(tenantId, userTenant.getTenantId());
    Assertions.assertEquals(email, userTenant.getEmail());

    UserTenantCollection userTenantCollection = userTenantClient.getAllUsersTenants();
    Assertions.assertEquals(4, userTenantCollection.getTotalRecords());
    sendAffiliationDeletedEvents(userTenantCollection.getUserTenants());
  }

  @Test
  void canSearchByUserNameCaseInsensitive() {
    String username = "CaSe-InSeNsItIvE";
    String lowerCaseUsername = "case-insensitive";
    String tenantId = "testTenant";
    String centralTenantId = "testTenant";
    String email = "test@mail.org";

    UserTenant affiliation = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername(username).withTenantId(tenantId).withEmail(email).withCentralTenantId(centralTenantId);

    int actualStatusCode = userTenantClient.attemptToSaveUserTenant(affiliation);
    Assertions.assertEquals(201, actualStatusCode);

    Map<String, String> params = Map.of("username", lowerCaseUsername);

    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    Assertions.assertEquals(1, collection.getTotalRecords());
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(username, userTenant.getUsername());
    Assertions.assertEquals(tenantId, userTenant.getTenantId());
    Assertions.assertEquals(email, userTenant.getEmail());

    UserTenantCollection userTenantCollection = userTenantClient.getAllUsersTenants();
    Assertions.assertEquals(4, userTenantCollection.getTotalRecords());
    sendAffiliationDeletedEvents(userTenantCollection.getUserTenants());
  }

  private void awaitHandlingEvent(int expectedSize) {
    Awaitility.await()
      .atMost(1, TimeUnit.MINUTES)
      .pollInterval(5, SECONDS)
      .until(() -> {
        UserTenantCollection collection = userTenantClient.getAllUsersTenants();
        return collection.getTotalRecords() == expectedSize;
      });
  }

  private void sendAffiliationCreatedEvent() {
    for (UserTenant userTenant : List.of(FIRST_AFFILIATION, SECOND_AFFILIATION, THIRD_AFFILIATION)) {
      String eventPayload = Json.encode(userTenant);
      sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName(),
        userTenant.getId(), eventPayload);
    }
    awaitHandlingEvent(3);
  }

  private void sendAffiliationUpdatedEvent(List<UserTenant> userTenants) {
    for (UserTenant userTenant : userTenants) {
      String eventPayload = Json.encode(userTenant);
      sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_UPDATED.getTopicName(),
        userTenant.getId(), eventPayload);
    }
    awaitHandlingEvent(3);
  }

  private void sendAffiliationDeletedEvents(List<UserTenant> userTenants) {
    for (UserTenant userTenant : userTenants) {
      String eventPayload = Json.encode(userTenant);
      sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_DELETED.getTopicName(),
        userTenant.getId(), eventPayload);
    }
    awaitHandlingEvent(0);
  }
}
