
package org.folio.rest.impl;

import io.vertx.core.json.Json;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import org.folio.domain.UserType;
import org.folio.event.UserEventType;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.UserTenantClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.*;
import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class UsersAPIConsortiaTest extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static UserTenantClient userTenantClient;

  @BeforeAll
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    userTenantClient.deleteAllUserTenants();
  }

  @Test
  void doNotSendUserCreateUpdateKafkaEventsForPatronUser() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    //That test checks scenario when we are in consortium mode, and for create/update events we are not sending events for patron users
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "patron");
    final var user = usersClient.createUser(userToCreate);
    User userToUpdate = createUser(userId, "joannek", "new_julia", "patron");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));
    usersClient.deleteUser(user.getId());

    List<UserEvent> userCreatedEvents = getUserEventsAndFilterByUserId(USER_CREATED, userId);
    List<UserEvent> userUpdatedEvents = getUserEventsAndFilterByUserId(USER_UPDATED, userId);
    List<UserEvent> userDeletedEvents = getUserEventsAndFilterByUserId(USER_DELETED, userId);

    assertEquals(0, userCreatedEvents.size());
    assertEquals(0, userUpdatedEvents.size());
    assertEquals(1, userDeletedEvents.size());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(404);
  }

  @Test
  void doNotSendUserCreateUpdateKafkaEventsForShadowUser() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    //That test checks scenario when we are in consortium mode, and for create/update events we are not sending events for shadow users
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "shadow");
    final var user = usersClient.createUser(userToCreate);
    User userToUpdate = createUser(userId, "joannek", "new_julia", "shadow");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));
    usersClient.deleteUser(user.getId());

    List<UserEvent> userCreatedEvents = getUserEventsAndFilterByUserId(USER_CREATED, userId);
    List<UserEvent> userUpdatedEvents = getUserEventsAndFilterByUserId(USER_UPDATED, userId);
    List<UserEvent> userDeletedEvents = getUserEventsAndFilterByUserId(USER_DELETED, userId);

    assertEquals(0, userCreatedEvents.size());
    assertEquals(0, userUpdatedEvents.size());
    assertEquals(1, userDeletedEvents.size());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(404);
  }

  @Test
  void canDeleteStaffAUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    final var user = usersClient.createUser(userToCreate);
    usersClient.deleteUser(user.getId());

    List<UserEvent> userCreatedEvents = getUserEventsAndFilterByUserId(USER_CREATED, userId);
    List<UserEvent> userDeletedEvents = getUserEventsAndFilterByUserId(USER_DELETED, userId);

    assertEquals(1, userCreatedEvents.size());
    assertEventContent(userCreatedEvents.get(0), UserEvent.Action.CREATE, user.getId());

    assertEquals(1, userDeletedEvents.size());
    assertEventContent(userDeletedEvents.get(0), UserEvent.Action.DELETE, user.getId());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(404);
  }

  @Test
  void sendAllEventsForSystemUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "system");
    final var user = usersClient.createUser(userToCreate);
    final User userToUpdate = createUser(userId, "joannek", "new_julia", "system");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));
    usersClient.deleteUser(user.getId());

    List<UserEvent> userCreatedEvents = getUserEventsAndFilterByUserId(USER_CREATED, userId);
    List<UserEvent> userUpdatedEvents = getUserEventsAndFilterByUserId(USER_UPDATED, userId);
    List<UserEvent> userDeletedEvents = getUserEventsAndFilterByUserId(USER_DELETED, userId);

    assertEquals(1, userCreatedEvents.size());
    assertEventContent(userCreatedEvents.get(0), UserEvent.Action.CREATE, user.getId());

    assertEquals(1, userUpdatedEvents.size());
    assertEventContent(userUpdatedEvents.get(0), UserEvent.Action.EDIT, user.getId());

    assertEquals(1, userDeletedEvents.size());
    assertEventContent(userDeletedEvents.get(0), UserEvent.Action.DELETE, user.getId());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(404);
  }

  @Test
  void canUpdateFirstNameForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    final User userToUpdate = createUser(userId, "joannek", "new_julia", "staff");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));

    Awaitility.await()
      .atMost(1, MINUTES)
      .pollInterval(5, SECONDS)
      .untilAsserted(() -> {
        final var updatedUser = usersClient.getUser(userId);
        assertThat(updatedUser.getPersonal().getFirstName(), is("new_julia"));
      });
  }

  @Test
  void cannotUpdateUserWithSameUsernameAsExistingUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, "user_test", "julia", "staff"))
      .statusCode(400)
      .body(is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "user_test", "julia", "staff");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(422)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotCreateUserWithoutUserTypeForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", null);
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(422)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotUpdateUserWithoutUserTypeForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, "joannek", "julia", null))
      .statusCode(400)
      .body(is(String.format("An invalid user type has been populated to a user, allowed values: %s",
        Arrays.stream(UserType.values()).map(UserType::getTypeName).toList())));
  }

  @Test
  void cannotCreateUserWithInvalidUserTypeForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "invalidType");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(422)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotUpdateUserWithInvalidUserTypeForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
//    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, "joannek", "julia", "invalidType"))
      .statusCode(400)
      .body(is(String.format("An invalid user type has been populated to a user, allowed values: %s",
        Arrays.stream(UserType.values()).map(UserType::getTypeName).toList())));
  }

  private List<UserEvent> getUserEventsAndFilterByUserId(UserEventType eventType, String userId) {
    List<UserEvent> usersList = getUserEvents(eventType);
    return usersList.stream()
      .filter(userEvent -> userId.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());
  }

  private List<UserEvent> getUserEvents(UserEventType eventType) {
    List<String> usersList = checkKafkaEventSent(TENANT_NAME, eventType.getTopicName());
    return usersList.stream()
      .map(s -> Json.decodeValue(s, UserEvent.class))
      .collect(Collectors.toList());
  }

  private void assertEventContent(UserEvent userEvent, UserEvent.Action action, String userId) {
    assertEquals(action, userEvent.getAction());
    assertEquals(TENANT_NAME, userEvent.getTenantId());
    assertEquals(userId, userEvent.getUser().getId());
  }

  private UserTenant getUserTenant() {
    return new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");
  }

  private User createUser(String userId, String username, String firstName, String type) {
    return User.builder()
      .id(userId)
      .username(username)
      .active(true)
      .type(type)
      .personal(Personal.builder()
        .firstName(firstName)
        .preferredFirstName("jules")
        .lastName("test")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();
  }
}
