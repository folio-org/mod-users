
package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.event.UserEventType.USER_CREATED;
import static org.folio.event.UserEventType.USER_DELETED;
import static org.folio.event.UserEventType.USER_UPDATED;
import static org.folio.extensions.KafkaContainerExtension.getTopicName;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.folio.support.matchers.DomainEventAssertions.await;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.matchesPattern;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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
import org.folio.support.kafka.FakeKafkaConsumer;
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
class UsersAPIConsortiaIT extends AbstractRestTestNoData {

  private static final String userCreatedTopic = getTopicName(TENANT_NAME, USER_CREATED.getTopicName());
  private static final String userUpdatedTopic = getTopicName(TENANT_NAME, USER_UPDATED.getTopicName());
  private static final String userDeletedTopic = getTopicName(TENANT_NAME, USER_DELETED.getTopicName());

  private static UsersClient usersClient;
  private static UserTenantClient userTenantClient;
  private static FakeKafkaConsumer kafkaConsumer;

  @BeforeAll
  static void beforeAll(Vertx vertx) {
    kafkaConsumer = new FakeKafkaConsumer()
      .consume(vertx, userCreatedTopic, userUpdatedTopic, userDeletedTopic);

    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    kafkaConsumer.removeAllEvents();
  }

  @AfterAll
  static void afterAll(VertxTestContext context) {
    kafkaConsumer.closeAsync().onComplete(context.succeedingThenComplete());
  }

  @Test
  void doNotSendUserCreateUpdateKafkaEventsForPatronUser() {
    //That test checks scenario when we are in consortium mode, and for create/update events we are not sending events for patron users
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "patron");
    final var user = usersClient.createUser(userToCreate);
    User userToUpdate = createUser(userId, "joannek", "new_julia", "patron");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(SC_NO_CONTENT);
    usersClient.deleteUser(user.getId());

    await().until(() -> getUserEvents(USER_DELETED, userId), hasSize(1));
    await(3).until(() -> getUserEvents(USER_CREATED, userId), hasSize(0));
    await().until(() -> getUserEvents(USER_UPDATED, userId), hasSize(0));

    usersClient.attemptToGetUser(user.getId())
      .statusCode(SC_NOT_FOUND);
  }

  @Test
  void doNotSendUserCreateUpdateKafkaEventsForShadowUser() {
    //That test checks scenario when we are in consortium mode, and for create/update events we are not sending events for shadow users
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "shadow");
    final var user = usersClient.createUser(userToCreate);
    User userToUpdate = createUser(userId, "joannek", "new_julia", "shadow");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(SC_NO_CONTENT);
    usersClient.deleteUser(user.getId());

    await().until(() -> getUserEvents(USER_DELETED, userId), hasSize(1));
    await(3).until(() -> getUserEvents(USER_CREATED, userId), hasSize(0));
    await().until(() -> getUserEvents(USER_UPDATED, userId), hasSize(0));

    usersClient.attemptToGetUser(user.getId())
      .statusCode(SC_NOT_FOUND);
  }

  @Test
  void canDeleteStaffAUserForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    final var user = usersClient.createUser(userToCreate);
    usersClient.deleteUser(user.getId());

    var userCreatedEvents = await().until(() -> getUserEvents(USER_CREATED, userId), hasSize(1));
    var userDeletedEvents = await().until(() -> getUserEvents(USER_DELETED, userId), hasSize(1));

    assertEquals(1, userCreatedEvents.size());
    assertEventContent(userCreatedEvents.getFirst(), UserEvent.Action.CREATE, user.getId());

    assertEquals(1, userDeletedEvents.size());
    assertEventContent(userDeletedEvents.getFirst(), UserEvent.Action.DELETE, user.getId());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(SC_NOT_FOUND);
  }

  @Test
  void sendAllEventsForSystemUserForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "system");
    final var user = usersClient.createUser(userToCreate);
    final User userToUpdate = createUser(userId, "joannek", "new_julia", "system");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(SC_NO_CONTENT);
    usersClient.deleteUser(user.getId());

    var userCreatedEvents = await().until(() -> getUserEvents(USER_CREATED, userId), hasSize(1));
    var userUpdatedEvents = await().until(() -> getUserEvents(USER_UPDATED, userId), hasSize(1));
    var userDeletedEvents = await().until(() -> getUserEvents(USER_DELETED, userId), hasSize(1));

    assertEquals(1, userCreatedEvents.size());
    assertEventContent(userCreatedEvents.getFirst(), UserEvent.Action.CREATE, user.getId());

    assertEquals(1, userUpdatedEvents.size());
    assertEventContent(userUpdatedEvents.getFirst(), UserEvent.Action.EDIT, user.getId());

    assertEquals(1, userDeletedEvents.size());
    assertEventContent(userDeletedEvents.getFirst(), UserEvent.Action.DELETE, user.getId());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(SC_NOT_FOUND);
  }

  @Test
  void canUpdateNotStaffUserWithoutUsernameForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "system");
    usersClient.createUser(userToCreate);
    final User userToUpdate = createUser(userId, null, "new_julia", "system");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(SC_NO_CONTENT);

    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(userId);
      assertThat(updatedUser.getPersonal().getFirstName(), is("new_julia"));
      assertNull(updatedUser.getUsername());
    });
  }

  @Test
  void canUpdateNameAndEmailForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "julia@email.com", "staff");
    usersClient.createUser(userToCreate);
    final User userToUpdate = createUser(userId, "joannek", "new_julia", "new_julia@email.com", "staff");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(SC_NO_CONTENT);

    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(userId);
      assertThat(updatedUser.getPersonal().getFirstName(), is("new_julia"));
      assertThat(updatedUser.getPersonal().getEmail(), is("new_julia@email.com"));
    });
  }

  @Test
  void cannotUpdateUserWithSameUsernameAsExistingUserForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, "user_test", "julia", "staff"))
      .statusCode(SC_BAD_REQUEST)
      .body(is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUserForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "user_test", "julia", "staff");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotCreateUserWithSameUsernameInUpperCase() {
    UserTenant userTenant = getUserTenant();
    userTenant.setUsername("uSeR_tEsT");
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    String usernameInUpperCase = "User_Test";
    final User userToCreate = createUser(userId, usernameInUpperCase, "julia", "staff");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotCreateUserWithoutUserTypeForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", null);
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotCreateUserWithoutUsernameForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, null, "julia", "staff");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(400)
      .body(matchesPattern("The user with the ID .* must have a username since consortium mode is enabled"));
  }

  @Test
  void cannotUpdateUserWithoutUsernameForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, null, "julia", "staff"))
      .statusCode(400)
      .body(matchesPattern("The user with the ID .* must have a username since consortium mode is enabled"));
  }

  @Test
  void cannotUpdateUserWithoutUserTypeForConsortia() {
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
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "invalidType");
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);
  }

  @Test
  void cannotUpdateUserWithInvalidUserTypeForConsortia() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);
    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);
    usersClient.attemptToUpdateUser(createUser(userId, "joannek", "julia", "invalidType"))
      .statusCode(SC_BAD_REQUEST)
      .body(is(String.format("An invalid user type has been populated to a user, allowed values: %s",
        Arrays.stream(UserType.values()).map(UserType::getTypeName).toList())));
  }

  @Test
  void shouldSendEventAfterChangingUserTypeFromStaffToPatron() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "staff");
    usersClient.createUser(userToCreate);

    final User userToUpdate = createUser(userId, "joannek", "julia", "patron");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(SC_NO_CONTENT));

    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(userId);
      assertThat(updatedUser.getType(), is("patron"));
    });

    List<UserEvent> userUpdatedEvents = getUserEvents(USER_UPDATED, userId);
    assertEquals(1, userUpdatedEvents.size());
    UserEvent userEvent = userUpdatedEvents.getFirst();
    assertEventContent(userEvent, UserEvent.Action.EDIT, userToUpdate.getId());
    assertEquals(UserType.PATRON.getTypeName(), userEvent.getUser().getType());
  }

  @Test
  void shouldSendEventAfterChangingUserTypeFromPatronToStaff() {
    UserTenant userTenant = getUserTenant();
    userTenantClient.attemptToSaveUserTenant(userTenant);

    String userId = UUID.randomUUID().toString();
    final User userToCreate = createUser(userId, "joannek", "julia", "patron");
    usersClient.createUser(userToCreate);

    final User userToUpdate = createUser(userId, "joannek", "julia", "staff");

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));

    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(userId);
      assertThat(updatedUser.getType(), is("staff"));
    });

    List<UserEvent> userUpdatedEvents = getUserEvents(USER_UPDATED, userId);
    assertEquals(1, userUpdatedEvents.size());
    UserEvent userEvent = userUpdatedEvents.getFirst();
    assertEventContent(userEvent, UserEvent.Action.EDIT, userToUpdate.getId());
    assertEquals(UserType.STAFF.getTypeName(), userEvent.getUser().getType());
  }

  private static List<UserEvent> getUserEvents(UserEventType eventType, String userId) {
    var topicName = getTopicName(TENANT_NAME, eventType.getTopicName());
    var events = kafkaConsumer.getEvents(topicName, userId);
    return events.stream()
      .map(KafkaConsumerRecord::value)
      .map(msgValue -> Json.decodeValue(msgValue.encode(), UserEvent.class))
      .toList();
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

  private User createUser(String userId, String username, String firstName, String email, String type) {
    return createUserBuilder(userId, username, type)
      .personal(createPersonalBuilder(firstName).email(email).build())
      .build();
  }

  private User createUser(String userId, String username, String firstName, String type) {
    return createUserBuilder(userId, username, type)
      .personal(createPersonalBuilder(firstName).build())
      .build();
  }

  private User.UserBuilder createUserBuilder(String userId, String username, String type) {
    return User.builder()
      .id(userId)
      .username(username)
      .active(true)
      .type(type)
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build());
  }

  private Personal.PersonalBuilder createPersonalBuilder(String firstName) {
    return Personal.builder()
      .firstName(firstName)
      .preferredFirstName("jules")
      .lastName("test");
  }
}
