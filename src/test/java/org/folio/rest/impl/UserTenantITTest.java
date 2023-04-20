package org.folio.rest.impl;

import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.event.ConsortiumEventType;
import org.folio.moduserstest.AbstractRestTest;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.support.http.UserTenantClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@ExtendWith(VertxExtension.class)
class UserTenantITTest extends AbstractRestTest {

  private static UserTenantClient userTenantClient;
  private static final UserTenant USER_AFFILIATION =  new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUserName("folio_user")
    .withTenantId("folio_tenant");

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    LOAD_SAMPLE_DATA = false;
    LOAD_REFERENCE_DATA = false;
  }

  @Test
  void canCreateUserTenant() {
    //given
    String eventPayload = Json.encode(USER_AFFILIATION);
    //when
    sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName(),
      USER_AFFILIATION.getId(), eventPayload);
    awaitHandlingEvent();
    //then
    UserTenantCollection collection = userTenantClient.getAllUsersTenants();
    Assertions.assertEquals(1, collection.getTotalRecords());
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertNull(userTenant.getIsPrimary());
    Assertions.assertNull(userTenant.getTenantName());
    Assertions.assertEquals(USER_AFFILIATION.getId(), userTenant.getId());
    Assertions.assertEquals(USER_AFFILIATION.getUserId(), userTenant.getUserId());
    Assertions.assertEquals(USER_AFFILIATION.getUserName(), userTenant.getUserName());
    Assertions.assertEquals(USER_AFFILIATION.getTenantId(), userTenant.getTenantId());
  }

  @Test
  void canSearchByUserName() {
    //given
    String username = USER_AFFILIATION.getUserName();
    String eventPayload = Json.encode(USER_AFFILIATION);
    Map<String, String> params = Map.of("username", username);
    //when
    sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName(),
      USER_AFFILIATION.getId(), eventPayload);
    awaitHandlingEvent();
    //then
    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(1, collection.getTotalRecords());
    Assertions.assertEquals(username, userTenant.getUserName());
  }

  @Test
  void canSearchByUserNameAndTenantId() {
    //given
    String username = USER_AFFILIATION.getUserName();
    String tenantId = USER_AFFILIATION.getTenantId();
    Map<String, String> params = Map.of(
      "username", username,
      "tenantId", tenantId);
    String eventPayload = Json.encode(USER_AFFILIATION);
    //when
    sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName(),
      USER_AFFILIATION.getId(), eventPayload);
    awaitHandlingEvent();
    //then
    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    Assertions.assertEquals(1, collection.getTotalRecords());
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(username, userTenant.getUserName());
    Assertions.assertEquals(tenantId, userTenant.getTenantId());
  }

  private void awaitHandlingEvent() {
    Awaitility.await()
      .atMost(1, TimeUnit.MINUTES)
      .pollInterval(5, TimeUnit.SECONDS)
      .until(() -> {
        UserTenantCollection collection = userTenantClient.getAllUsersTenants();
        return CollectionUtils.isNotEmpty(collection.getUserTenants());
      });
  }
}