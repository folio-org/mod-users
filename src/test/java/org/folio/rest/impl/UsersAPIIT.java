
package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.*;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.folio.moduserstest.AbstractRestTestNoData;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.folio.event.UserEventType;
import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UserTenantClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.testcontainers.shaded.org.awaitility.Awaitility;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class UsersAPIIT extends AbstractRestTestNoData {

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
  void canCreateUser() {
    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .id("999fd1a4-1865-4991-ae9d-6c9f75d4b043")
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var createdUser = usersClient.createUser(userToCreate);

    assertThat(createdUser.getId(), is(notNullValue()));
    final var personal = createdUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));

    assertThat(createdUser.getTags().getTagList(), containsInAnyOrder("foo", "bar"));
    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void canGetAUser() {
    final var homeAddressType = addressTypesClient.createAddressType(
      AddressType.builder()
        .addressType("Home")
        .build());

    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(homeAddressType.getId()).build()))
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var createdUserId = usersClient.createUser(userToCreate).getId();

    final var fetchedUser = usersClient.getUser(createdUserId);

    assertThat(fetchedUser.getId(), is(notNullValue()));
    assertThat(fetchedUser.getUsername(), is("juliab"));
    assertThat(fetchedUser.getActive(), is(true));

    final var personal = fetchedUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));
    assertThat(personal.getAddresses().size(), is(1));

    assertThat(fetchedUser.getTags().getTagList(),
      containsInAnyOrder("foo", "bar"));

    assertThat(fetchedUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(fetchedUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void canCreateMultipleUsersWithoutUsername() {
    usersClient.createUser(User.builder()
      .build());

    usersClient.attemptToCreateUser(User.builder()
      .build())
      .statusCode(is(201));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUser() {
    usersClient.createUser("julia");

    final var errors = usersClient.attemptToCreateUser(User.builder()
      .username("julia")
      .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameBarcodeAsExistingUser() {
    usersClient.createUser(User.builder()
      .barcode("12345")
      .build());

    final var errors = usersClient.attemptToCreateUser(User.builder()
      .barcode("12345")
      .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("This barcode has already been taken"));
  }

  @Test
  void cannotCreateUserWithSameIdAsExistingUser() {
    final var existingUser = usersClient.createUser(User.builder()
      .username("julia")
      .build());

    final var errors = usersClient.attemptToCreateUser(User.builder()
      .id(existingUser.getId())
      .username("steve")
      .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("User with this id already exists"));
  }

  static Stream<Arguments> dateOfBirth() {
    return Stream.of(
        Arguments.of("0000-01-01", false),
        Arguments.of("0000-12-31", false),
        Arguments.of("0001-01-01", true),
        Arguments.of("1900-01-01", true),
        Arguments.of("2000-01-01", true),
        Arguments.of("1-1-1", false)
    );
  }

  @ParameterizedTest
  @MethodSource("dateOfBirth")
  void dateOfBirthPost(String dateOfBirth, boolean successExpected) {
    var user = User.builder()
        .personal(Personal.builder().lastName("Last").dateOfBirth(dateOfBirth).build())
        .build();
    usersClient.attemptToCreateUser(user)
    .statusCode(is(successExpected ? 201 : 400));
  }

  @ParameterizedTest
  @MethodSource("dateOfBirth")
  void dateOfBirthPut(String dateOfBirth, boolean successExpected) {
    var id = UUID.randomUUID().toString();
    usersClient.attemptToCreateUser(User.builder().id(id).build())
    .statusCode(201);
    var user = User.builder()
        .id(id)
        .personal(Personal.builder().lastName("Last").dateOfBirth(dateOfBirth).build())
        .build();
    usersClient.attemptToUpdateUser(user)
    .statusCode(is(successExpected ? 204 : 400));
  }

  @Test
  void cannotCreateUserWithAddressButNoAddressType() {
    final var addressWithoutId = Address.builder().build();
    final var homeAddressType = addressTypesClient.createAddressType(
      AddressType.builder()
        .addressType("Home")
        .build());
    final var homeAddress = Address.builder()
      .addressTypeId(homeAddressType.getId()).build();

    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .addresses(List.of(homeAddress, addressWithoutId))
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(is(422));
  }

  @Test
  @SneakyThrows
  void canHandleDatabaseException() {
    var id = UUID.randomUUID().toString();
    PostgresClient.getInstance(Vertx.vertx())
    .execute("ALTER TABLE " + TENANT_NAME + "_mod_users.users ADD CHECK (id <> '" + id + "')")
    .toCompletionStage().toCompletableFuture().get(5, SECONDS);
    usersClient.attemptToCreateUser(User.builder().id(id).build())
    .statusCode(is(500));
  }

  @Test
  void canUpdateAUser() {
    final var user = usersClient.createUser(User.builder()
      .username("julia")
      .build());

    usersClient.attemptToUpdateUser(User.builder()
        .id(user.getId())
        .username("julia-brockhurst")
        .build())
      .statusCode(is(204));

    Awaitility.await()
      .atMost(1, MINUTES)
      .pollInterval(5, SECONDS)
      .untilAsserted(() -> {
        final var updatedUser = usersClient.getUser(user.getId());
        assertThat(updatedUser.getUsername(), is("julia-brockhurst"));
      });
  }

  @Test
  void cannotUpdateAUserThatDoesNotExist() {
    // Create a user to ensure this isn't updated unintentionally
    usersClient.createUser(User.builder()
      .username("julia")
      .build());

    usersClient.attemptToUpdateUser(User.builder()
        .id(UUID.randomUUID().toString())
        .username("julia-brockhurst")
        .build())
      .statusCode(is(404));
  }

  @Test
  void cannotUpdateAUserWithUsernameThatAlreadyExists() {
    usersClient.createUser(User.builder()
      .username("a-username")
      .build());

    final var anotherUser = usersClient.createUser(User.builder()
      .username("another-username")
      .build());

    usersClient.attemptToUpdateUser(
      User.builder()
        .id(anotherUser.getId())
        .username("a-username")
        .build())
      .statusCode(is(400))
      .body(is("User with this username already exists"));
  }

  @Test
  void cannotUpdateAUserWithBarcodeThatAlreadyExists() {
    usersClient.createUser(User.builder()
      .username("some-user")
      .barcode("54396735869")
      .personal(Personal.builder().lastName("some-user").email("test@mail.org").build())
      .build());

    final var anotherUser = usersClient.createUser(User.builder()
      .username("another-user")
      .personal(Personal.builder().lastName("another-user").email("test@mail.org").build())
      .build());

    usersClient.attemptToUpdateUser(
        User.builder()
          .id(anotherUser.getId())
          .username("another-user")
          .barcode("54396735869")
          .personal(Personal.builder().lastName("another-user").email("test@mail.org").build())
          .build())
      .statusCode(is(400))
      .body(is("This barcode has already been taken"));
  }

  @Test
  void cannotChangeAUsersId() {
    final var julia = usersClient.createUser(User.builder()
      .username("julia")
      .build());

    usersClient.attemptToUpdateUser(julia.getId(),
      User.builder()
        .id(UUID.randomUUID().toString())
        .username("julia")
        .build())
      .statusCode(is(400))
      .body(is("You cannot change the value of the id field"));
  }

  @Test
  void canFindUserByUsername() {
    final var steve = usersClient.createUser(User.builder()
      .username("steve")
      .build());

    usersClient.createUser(User.builder()
      .username("joanne")
      .build());

    final var foundUsers = usersClient.getUsers("username==\"steve\"");

    assertThat(foundUsers.getTotalRecords(), is(1));
    assertThat(foundUsers.getFirstUser().getUsername(), is("steve"));
    assertThat(foundUsers.getFirstUser().getId(), is(steve.getId()));
  }

  @Test
  void canFindUserById() {
    final var steve = usersClient.createUser(User.builder()
      .username("steve")
      .build());

    usersClient.createUser(User.builder()
      .username("joanne")
      .build());

    final var foundUsers = usersClient.getUsers(
      String.format("id==\"%s\"", steve.getId()));

    assertThat(foundUsers.getTotalRecords(), is(1));
    assertThat(foundUsers.getFirstUser().getUsername(), is("steve"));
    assertThat(foundUsers.getFirstUser().getId(), is(steve.getId()));
  }

  @Test
  void canFindUserByPreferredFirstName() {
    usersClient.createUser(User.builder()
      .username("steve")
      .build());

    usersClient.createUser(User.builder()
      .username("wilson")
      .active(true)
      .personal(Personal.builder()
        .firstName("wilson")
        .preferredFirstName("will")
        .lastName("anderson")
        .build())
      .build());

    final var userToFind = User.builder()
      .username("juliab")
      .active(true)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .build();

    final var userToFindId = usersClient.createUser(userToFind).getId();
    final var foundUsers = usersClient.getUsers("personal.preferredFirstName==\"jules\"");

    assertThat(foundUsers.getTotalRecords(), is(1));
    assertThat(foundUsers.getFirstUser().getUsername(), is("juliab"));
    assertThat(foundUsers.getFirstUser().getId(), is(userToFindId));
  }

  @Test
  void canSearchForUsers() {
    final var steve = usersClient.createUser(User.builder()
      .username("steve")
      .active(true)
      .build());

    usersClient.createUser(User.builder()
      .username("joanne")
      .active(true)
      .build());

    final var typicalSearchFromUI = "(((username=\"ste*\" or personal.firstName=\"ste*\" or "
      + "personal.lastName=\"ste*\" or personal.email=\"ste*\" or barcode=\"ste*\" or "
      + "id=\"ste*\" or externalSystemId=\"ste*\")) and active=\"true\") "
      + "sortby personal.lastName personal.firstName";

    final var foundUsers = usersClient.getUsers(typicalSearchFromUI);

    assertThat(foundUsers.getTotalRecords(), is(1));
    assertThat(foundUsers.getFirstUser().getUsername(), is("steve"));
    assertThat(foundUsers.getFirstUser().getId(), is(steve.getId()));
  }

  @Test
  void cannotSearchUsingInvalidCQL() {
    usersClient.attemptToGetUsers("username==")
      .statusCode(is(HTTP_BAD_REQUEST))
      .body(containsString("expected index or term, got EOF"));
  }

  @Test
  void canFindActiveUsers() {
    usersClient.createUser(User.builder()
      .username("steve")
      .active(true)
      .build());

    usersClient.createUser(User.builder()
      .username("joanne")
      .active(false)
      .build());

    usersClient.createUser(User.builder()
      .username("jenna")
      .active(true)
      .build());

    final var activeUsers = usersClient.getUsers("active=true");

    assertThat(activeUsers.getTotalRecords(), is(2));
  }

  @Test
  void canDeleteAUser() {
    commitAllMessagesInTopic(TENANT_NAME, USER_CREATED.getTopicName());
    commitAllMessagesInTopic(TENANT_NAME, USER_DELETED.getTopicName());
    String userId = UUID.randomUUID().toString();
    final var userToCreate = User.builder()
      .id(userId)
      .username("joannek")
      .active(true)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var user = usersClient.createUser(userToCreate);
    usersClient.deleteUser(user.getId());

    usersClient.attemptToGetUser(user.getId())
      .statusCode(404);
  }

  @Test
  void canDeleteAPatronUser() {
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
      .type("patron")
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

    assertEquals(0, userCreatedEvents.size());

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
      .personal(Personal.builder()
        .firstName("new_julia")
        .lastName("new_brockhurst")
        .preferredFirstName("jules")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(204));

    List<UserEvent> userCreatedEvents = getUserEventsAndFilterByUserId(USER_CREATED, userId);
    List<UserEvent> userUpdatedEvents = getUserEventsAndFilterByUserId(USER_UPDATED, userId);

    assertEquals(1, userCreatedEvents.size());
    assertEventContent(userCreatedEvents.get(0), UserEvent.Action.CREATE, userId);

    assertEquals(1, userUpdatedEvents.size());
    assertEventContent(userUpdatedEvents.get(0), UserEvent.Action.EDIT, userId);

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
  void cannotDeleteAUserThatDoesNotExist() {
    // Define another user to make sure it isn't deleted by accident
    createUser("joannek");

    usersClient.attemptToDeleteUser(UUID.randomUUID().toString())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void canDeleteMultipleUsersUsingCQL() {
    final var user1 = createUser("1234");
    final var user2 = createUser("201");
    final var user3 = createUser("1999");

    deleteUsersByUsername("1*");

    usersClient.attemptToGetUser(user2.getId())
      .statusCode(200);

    usersClient.attemptToGetUser(user1.getId())
      .statusCode(404);

    usersClient.attemptToGetUser(user3.getId())
      .statusCode(404);
  }

  User createUser(String username) {
    return usersClient.createUser(User.builder()
      .username(username)
      .build());
  }

  private void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
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
