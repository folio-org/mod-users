
package org.folio.rest.impl;

import io.vertx.core.json.Json;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.folio.event.UserEventType;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.support.*;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UserTenantClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;

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
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;
  private static UserTenantClient userTenantClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
    addressTypesClient.deleteAllAddressTypes();
    userTenantClient.deleteAllUserTenants();
  }

  @Test
  void doNotSendUserCreateUpdateKafkaEvents() {
    //That test checks scenario when we are in consortium mode, and for create/update events we are not sending events for patron users
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("patron")
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var user = usersClient.createUser(userToCreate);

    User userToUpdate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("patron")
      .personal(Personal.builder()
        .firstName("new_julia")
        .lastName("new_brockhurst")
        .preferredFirstName("jules")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

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
  void canDeleteAUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

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
  void canUpdateUserNameForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_UPDATED.getTopicName());
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();

    final User userToCreate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("julia")
        .lastName("brockhurst")
        .preferredFirstName("jules")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.createUser(userToCreate);

    User userToUpdate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("new_julia")
        .lastName("new_brockhurst")
        .preferredFirstName("jules")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));

    Awaitility.await()
      .atMost(1, MINUTES)
      .pollInterval(5, SECONDS)
      .untilAsserted(() -> {
        final var updatedUser = usersClient.getUser(userId);
        assertThat(updatedUser.getPersonal().getFirstName(), is("new_julia"));
        assertThat(updatedUser.getPersonal().getLastName(), is("new_brockhurst"));
      });
  }

  @Test
  void cannotUpdateUserWithSameUsernameAsExistingUserForConsortia() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var user = usersClient.createUser(userToCreate);

    usersClient.attemptToUpdateUser(
        User.builder()
          .id(user.getId())
          .username("user_test")
          .type("staff")
          .build())
      .statusCode(400)
      .body(is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUserForConsortia() {
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("user_test")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("user")
        .preferredFirstName("jules")
        .lastName("test")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(422)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotCreateUserWithoutUserTypeForConsortia() {
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("user_test")
      .active(true)
      .personal(Personal.builder()
        .firstName("user")
        .preferredFirstName("jules")
        .lastName("test")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(422)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotUpdateUserWithoutUserTypeForConsortia() {
    UserTenant userTenant = new UserTenant()
      .withId(UUID.randomUUID().toString())
      .withUserId(UUID.randomUUID().toString())
      .withUsername("user_test").withTenantId("tenant_test").withCentralTenantId("diku");

    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = User.builder()
      .id(userId)
      .username("userTest")
      .active(true)
      .type("staff")
      .personal(Personal.builder()
        .firstName("user")
        .preferredFirstName("jules")
        .lastName("test")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.createUser(userToCreate);

    usersClient.attemptToUpdateUser(
      User.builder()
        .id(userId)
        .username("user_test")
        .active(true)
        .personal(Personal.builder()
          .firstName("user")
          .preferredFirstName("jules")
          .lastName("test")
          .build())
        .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
        .build())
      .statusCode(400)
      .body(is("The user type was not populated for the user"));
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
}
