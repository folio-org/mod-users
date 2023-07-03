
package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.event.UserEventType.USER_CREATED;
import static org.folio.event.UserEventType.USER_DELETED;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
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
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.Assertions;
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

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class UsersAPIIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
    addressTypesClient.deleteAllAddressTypes();
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

    List<UserEvent> usersList = getUserEventsAndFilterByUserId(USER_CREATED, createdUser.getId());
    var userFromEventPayload = usersList.get(0).getUser();

    assertEquals(1, usersList.size());
    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getUsername(), allOf(is("juliab"), equalTo(userFromEventPayload.getUsername())));
    assertThat(createdUser.getActive(), allOf(is(true), equalTo(userFromEventPayload.getActive())));

    final var personal = createdUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));
    assertThat(userFromEventPayload.getPersonal(), is(nullValue()));

    assertThat(createdUser.getTags().getTagList(),
      containsInAnyOrder("foo", "bar"));
    assertThat(userFromEventPayload.getTags().getTagList(),
      containsInAnyOrder("foo", "bar"));

    Assertions.assertNull(userFromEventPayload.getMetadata());
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

    final var updatedUser = usersClient.getUser(user.getId());

    assertThat(updatedUser.getUsername(), is("julia-brockhurst"));
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
      .build());

    final var anotherUser = usersClient.createUser(User.builder()
      .username("another-user")
      .build());

    usersClient.attemptToUpdateUser(
        User.builder()
          .id(anotherUser.getId())
          .username("another-user")
          .barcode("54396735869")
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
    String userId1 = user1.getId();
    String userId2 = user2.getId();
    String userId3 = user3.getId();

    deleteUsersByUsername("1*");

    List<UserEvent> userCreatedEvents = getUserEvents(USER_CREATED);
    List<UserEvent> userCreatedEventsForUser1 = userCreatedEvents.stream().filter(userEvent -> userId1.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());
    List<UserEvent> userCreatedEventsForUser2 = userCreatedEvents.stream().filter(userEvent -> userId2.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());
    List<UserEvent> userCreatedEventsForUser3 = userCreatedEvents.stream().filter(userEvent -> userId3.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());

    List<UserEvent> userDeletedEvents = getUserEvents(USER_DELETED);
    List<UserEvent> userDeletedEventsForUser1 = userDeletedEvents.stream().filter(userEvent -> userId1.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());
    List<UserEvent> userDeletedEventsForUser3 = userDeletedEvents.stream().filter(userEvent -> userId3.equals(userEvent.getUser().getId()))
      .collect(Collectors.toList());

    assertEquals(1, userCreatedEventsForUser1.size());
    assertEventContent(userCreatedEventsForUser1.get(0), UserEvent.Action.CREATE, user1.getId());

    assertEquals(1, userCreatedEventsForUser2.size());
    assertEventContent(userCreatedEventsForUser2.get(0), UserEvent.Action.CREATE, user2.getId());

    assertEquals(1, userCreatedEventsForUser3.size());
    assertEventContent(userCreatedEventsForUser3.get(0), UserEvent.Action.CREATE, user3.getId());

    assertEquals(1, userDeletedEventsForUser1.size());
    assertEventContent(userDeletedEventsForUser1.get(0), UserEvent.Action.DELETE, user1.getId());

    assertEquals(1, userDeletedEventsForUser3.size());
    assertEventContent(userDeletedEventsForUser3.get(0), UserEvent.Action.DELETE, user3.getId());

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
