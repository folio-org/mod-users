package org.folio.rest.impl;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.event.ConsortiumEventType;
import org.folio.event.service.UserTenantService;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.support.http.UserTenantClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@ExtendWith(VertxExtension.class)
class UserTenantIT extends AbstractRestTestNoData {

  private static UserTenantClient userTenantClient;
  @Autowired
  private static UserTenantService userTenantService = new UserTenantService();
  private static final UserTenant USER_AFFILIATION =  new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUserName("folio_user")
    .withTenantId("folio_tenant");
  private static final UserTenant USER_AFFILIATION_TO_DELETE =  new UserTenant()
    .withId(UUID.randomUUID().toString())
    .withUserId(UUID.randomUUID().toString())
    .withUserName("folio_user")
    .withTenantId("folio_tenant");


  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @Test
  void canCreateUserTenant() {
    //when
    sendAffiliationCreatedEvent();
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

//  @Test
//  void canDeleteUserTenant() {
////    String id = UUID.randomUUID().toString();
////    UserTenant userTenant = createUserTenant(id, "dddd");
////    userTenantService.saveUserTenant(userTenant, "diku", Vertx.vertx())
////      .onSuccess(aBoolean -> System.out.println("CCCCCCCCC"));
////    UserTenantCollection collection = userTenantClient.getAllUsersTenants();
////    Assertions.assertEquals(1, collection.getTotalRecords());
////    UserTenant userTenant1 = collection.getUserTenants().iterator().next();
////    System.out.println("USER ids are:--"+userTenant1.getUserId()+"-----"+id);
//    sendAffiliationDeletedEvent();
//    UserTenantCollection collection1 = userTenantClient.getAllUsersTenants();
//    Assertions.assertEquals(1, collection1.getTotalRecords());
//
//
//
//  }

  @Test
  void canSearchByUserName() {
    //given
    String username = USER_AFFILIATION.getUserName();
    Map<String, String> params = Map.of("username", username);
    //when
    sendAffiliationCreatedEvent();
    //then
    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(1, collection.getTotalRecords());
    Assertions.assertEquals(username, userTenant.getUserName());
  }

  @Test
  void canSearchByUserId() {
    //given
    String userId = USER_AFFILIATION.getUserId();
    Map<String, String> params = Map.of("userId", userId);
    //when
    sendAffiliationCreatedEvent();
    //then
    UserTenantCollection collection = userTenantClient.getUserTenants(params);
    UserTenant userTenant = collection.getUserTenants().iterator().next();
    Assertions.assertEquals(1, collection.getTotalRecords());
    Assertions.assertEquals(userId, userTenant.getUserId());
  }

  @Test
  void canSearchByUserNameAndTenantId() {
    //given
    String username = USER_AFFILIATION.getUserName();
    String tenantId = USER_AFFILIATION.getTenantId();
    Map<String, String> params = Map.of(
      "username", username,
      "tenantId", tenantId);
    //when
    sendAffiliationCreatedEvent();
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

  private void sendAffiliationCreatedEvent() {
    String eventPayload = Json.encode(USER_AFFILIATION);
    sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_CREATED.getTopicName(),
      USER_AFFILIATION.getId(), eventPayload);
    awaitHandlingEvent();
  }

  public UserTenant createUserTenant(String id, String userName) {
    return new UserTenant()
      .withUserName("Test1")
      .withIsPrimary(false)
      .withId(UUID.randomUUID().toString())
      .withUserId(id)
      .withTenantId("folio_tenant");
  }

  private void sendAffiliationDeletedEvent() {
    String eventPayload = Json.encode(USER_AFFILIATION_TO_DELETE);
    sendEvent(TENANT_NAME, ConsortiumEventType.CONSORTIUM_PRIMARY_AFFILIATION_DELETED.getTopicName(),
      USER_AFFILIATION.getId(), eventPayload);
    awaitHandlingEvent();
  }
}
