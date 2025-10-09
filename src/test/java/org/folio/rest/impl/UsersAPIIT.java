package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.apache.http.HttpStatus.SC_BAD_REQUEST;
import static org.apache.http.HttpStatus.SC_NOT_FOUND;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.PROGRAMS;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.SERVICES;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.SUPPORT;
import static org.folio.rest.utils.ManualBlockWiremockStubs.addManualBlockStubForDeleteUserById;
import static org.folio.rest.utils.ManualBlockWiremockStubs.blankManualBlockByCQLStubForDeleteUserById1;
import static org.folio.rest.utils.ManualBlockWiremockStubs.deleteManualBlockByIdStub500Error;
import static org.folio.rest.utils.ManualBlockWiremockStubs.manualBlockByCQLStubForDeleteUserById500Error;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.folio.support.kafka.DomainEventAssertions.assertCreateEvent;
import static org.folio.support.kafka.DomainEventAssertions.assertDeleteEvent;
import static org.folio.support.kafka.DomainEventAssertions.assertUpdateEvent;
import static org.folio.support.matchers.DomainEventAssertions.await;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static com.github.tomakehurst.wiremock.client.WireMock.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.time.ZonedDateTime;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.apache.commons.lang3.RandomStringUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import com.fasterxml.jackson.databind.JsonMappingException;

import org.folio.domain.UserType;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.persist.PostgresClient;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.ConfigurationClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.TimerInterfaceClient;
import org.folio.support.http.UserProfilePictureClient;
import org.folio.support.http.UsersClient;
import org.folio.support.kafka.FakeKafkaConsumer;
import org.folio.support.tags.IntegrationTest;
import lombok.SneakyThrows;

@IntegrationTest
@ExtendWith(VertxExtension.class)
class UsersAPIIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;
  private static UserProfilePictureClient userProfilePictureClient;
  private static ConfigurationClient configurationClient;
  private static TimerInterfaceClient timerInterfaceClient;
  protected static FakeKafkaConsumer kafkaConsumer;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
    userProfilePictureClient = new UserProfilePictureClient(okapiUrl, okapiHeaders);
    configurationClient = new ConfigurationClient(okapiUrl, okapiHeaders);
    timerInterfaceClient = new TimerInterfaceClient(okapiUrl, okapiHeaders);
    kafkaConsumer = new FakeKafkaConsumer().consume(module.getVertx());
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
    addressTypesClient.deleteAllAddressTypes();
    kafkaConsumer.removeAllEvents();
  }

  @AfterAll
  static void afterAll(VertxTestContext context) {
    kafkaConsumer.closeAsync().onComplete(context.succeedingThenComplete());
  }

  @Test
  void canCreateUser() {
    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .id("999fd1a4-1865-4991-ae9d-6c9f75d4b043")
      .personal(Personal.builder()
        .pronouns("He/Him")
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var createdUser = usersClient.createUser(userToCreate);
    await().until(() -> kafkaConsumer.getUsersEvents(userToCreate.getId()).size(), is(1));
    assertCreateEventForUser(createdUser);
    assertThat(createdUser.getId(), is(notNullValue()));
    final var personal = createdUser.getPersonal();

    assertThat(personal.getPronouns(), is("He/Him"));
    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));

    assertThat(createdUser.getTags().getTagList(), containsInAnyOrder("foo", "bar"));
    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void canNotCreateUser() {
    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .type(UserType.SHADOW.getTypeName())
      .id("999fd1a4-1865-4991-ae9d-6c9f75d4b043")
      .personal(Personal.builder()
        .firstName("julia")
        .profilePictureLink("/link")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();
    usersClient.attemptToCreateUser(userToCreate)
      .statusCode(is(500));
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
        .pronouns("He/Him")
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

    assertThat(personal.getPronouns(), is("He/Him"));
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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().getFirst().getMessage(),
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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().getFirst().getMessage(),
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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().getFirst().getMessage(),
      is("User with this id already exists"));
  }

  @Test
  void cannotCreateUserWithPreferedEmailEnums() {
    Set set = new LinkedHashSet<String>();
    set.add(SUPPORT);
    set.add(PROGRAMS);
    set.add(SERVICES);
    usersClient.attemptToCreateUser(User.builder()
        .id(UUID.randomUUID().toString())
        .username("steve123")
        .preferredEmailCommunication(set)
        .build())
      .statusCode(is(201));
  }

  @Test
  void cannotNotCreateUserWithRandomPreferedEmailEnums() {
    Set set = new LinkedHashSet();
    set.add("TEST");
    set.add(PROGRAMS);
    set.add(SERVICES);
    assertThrows(JsonMappingException.class, () -> {
      usersClient.attemptToCreateUser(User.builder()
        .id(UUID.randomUUID().toString())
        .username("steve1234")
        .preferredEmailCommunication(set)
        .build());
    });
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
      .statusCode(SC_UNPROCESSABLE_ENTITY);
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
    var id = UUID.randomUUID().toString();
    final var user = usersClient.createUser(User.builder()
      .id(id)
      .username("julia")
      .build());

    await().until(() -> kafkaConsumer.getUsersEvents(id).size(), is(1));

    usersClient.attemptToUpdateUser(User.builder()
        .id(user.getId())
        .username("julia-brockhurst")
        .build())
      .statusCode(is(204));
    await().until(() -> kafkaConsumer.getUsersEvents(id).size(), is(2));
    assertUpdateEventForUser(user);
    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(user.getId());
      assertThat(updatedUser.getUsername(), is("julia-brockhurst"));
    });
  }

  @Test
  void canNotUpdateAUser() {
    final var user = usersClient.createUser(User.builder()
      .username("julia")
      .type(UserType.SHADOW.getTypeName())
      .build());
    Personal personal = Personal.builder().lastName("mark").profilePictureLink("/link").build();

    usersClient.attemptToUpdateUser(User.builder()
        .id(user.getId())
        .username("julia-brockhurst")
        .personal(personal)
        .type(UserType.SHADOW.getTypeName())
        .build())
      .statusCode(is(500));
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
  void updateUserPublishesCorrectMetadataInKafkaEvent() {
    final var createdUser = usersClient.createUser(User.builder()
      .username("kafka-test-user")
      .barcode("12345")
      .active(true)
      .personal(Personal.builder().lastName("KafkaTest").firstName("User").build())
      .build());

    await().until(() -> kafkaConsumer.getUsersEvents(createdUser.getId()).size(), is(1));

    final var originalMetadata = createdUser.getMetadata();
    final var originalCreatedDate = originalMetadata.getCreatedDate();
    final var originalUpdatedDate = originalMetadata.getUpdatedDate();
    final var originalCreatedBy = originalMetadata.getCreatedByUserId();

    usersClient.attemptToUpdateUser(User.builder()
        .id(createdUser.getId())
        .username("kafka-test-user-updated")
        .barcode("67890")
        .active(false)
        .personal(Personal.builder().lastName("KafkaTest").firstName("UpdatedUser").build())
        .build())
      .statusCode(is(204));

    await().until(() -> kafkaConsumer.getUsersEvents(createdUser.getId()).size(), is(2));

    awaitUntilAsserted(() -> {
      final var updateEvent = kafkaConsumer.getLastUserEvent(createdUser.getId());
      final var data = updateEvent.value().getJsonObject("data");

      final var oldUser = data.getJsonObject("old");
      assertThat(oldUser.getString("username"), is("kafka-test-user"));
      assertThat(oldUser.getString("barcode"), is("12345"));
      assertThat(oldUser.getBoolean("active"), is(true));
      assertThat(oldUser.getJsonObject("personal").getString("firstName"), is("User"));

      final var newUser = data.getJsonObject("new");
      assertThat(newUser.getString("username"), is("kafka-test-user-updated"));
      assertThat(newUser.getString("barcode"), is("67890"));
      assertThat(newUser.getBoolean("active"), is(false));
      assertThat(newUser.getJsonObject("personal").getString("firstName"), is("UpdatedUser"));

      final var newUserMetadata = newUser.getJsonObject("metadata");
      final var createdDateInEvent = ZonedDateTime.parse(newUserMetadata.getString("createdDate"));
      final var createdByInEvent = newUserMetadata.getString("createdByUserId");
      final var updatedDateInEvent = ZonedDateTime.parse(newUserMetadata.getString("updatedDate"));

      assertThat(createdDateInEvent, is(originalCreatedDate));
      assertThat(updatedDateInEvent.isAfter(originalUpdatedDate), is(true));
      assertThat(createdByInEvent, is(originalCreatedBy));
    });
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
      .statusCode(SC_BAD_REQUEST)
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
  void canSearchForKeywordsAndSortByPatronGroup() {
    final var staff = groupsClient.createGroup(Group.builder()
      .id("22222222-9f30-428b-8f63-9fe35b818542").group("staff").build());
    final var student = groupsClient.createGroup(Group.builder()
      .id("11111111-9f30-428b-8f63-9fe35b818542").group("student").build());
    usersClient.createUser(User.builder()
      .username("stacy").patronGroup(student.getId()).build());
    usersClient.createUser(User.builder()
      .username("stephanie").patronGroup(student.getId()).build());
    usersClient.createUser(User.builder()
      .username("steve").patronGroup(staff.getId()).build());
    usersClient.createUser(User.builder()
      .username("steven").patronGroup(null).build());

    final var cql = "keywords=\"ste*\" sortBy patronGroup.group username";
    final var foundUsers = usersClient.getUsers(cql);

    assertThat(foundUsers.getUsers().size(), is(3));
    // should sort by patronGroup name. The two records have patronGroup id
    // and username that sort in opposite order of patronGroup name.
    assertThat(foundUsers.getUsers().getFirst().getUsername(), is("steve"));
    assertThat(foundUsers.getUsers().get(1).getUsername(), is("stephanie"));
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

    usersClient.deleteUser(user.getId(), addManualBlockStubForDeleteUserById(wireMockServer));

    await().until(() -> kafkaConsumer.getUsersEvents(userToCreate.getId()).size(), is(2));
    assertDeleteEventForUser(user);
    usersClient.attemptToGetUser(user.getId())
      .statusCode(SC_NOT_FOUND);

    // Verify that the mock was called
    wireMockServer.verify(getRequestedFor(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching(".*userId.*" + userId + ".*")));
    wireMockServer.verify(deleteRequestedFor(urlPathMatching("/manualblocks/.*")));

    // Clean up WireMock
    wireMockServer.resetRequests();
  }

  @Test
  void canDeleteAUser_manualBlockByCQL_Error500_negative() {
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

    usersClient.attemptToDeleteUser(user.getId(), manualBlockByCQLStubForDeleteUserById500Error(wireMockServer))
      .statusCode(500)
      .body(is("Failed to delete user error due to: Simulated error"));

    // Verify that the mock was called
    wireMockServer.verify(getRequestedFor(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching(".*userId.*" + userId + ".*")));

    // Clean up WireMock
    wireMockServer.resetRequests();
  }

  @Test
  void canDeleteAUser_delete_manualBlocks_Error500_negative() {
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

    usersClient.attemptToDeleteUser(user.getId(), deleteManualBlockByIdStub500Error(wireMockServer))
      .statusCode(500)
      .body(is("Failed to delete user error due to: Failed to delete manual block. Status code: 500, body: null"));

    // Verify that the mock was called
    wireMockServer.verify(getRequestedFor(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching(".*userId.*" + userId + ".*")));
    wireMockServer.verify(deleteRequestedFor(urlPathMatching("/manualblocks/.*")));

    // Clean up WireMock
    wireMockServer.resetRequests();
  }

  @Test
  void canDeleteAUser_delete_manualBlocks_EmptyResBodyError500_negative() {
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

    usersClient.attemptToDeleteUser(user.getId(), blankManualBlockByCQLStubForDeleteUserById1(wireMockServer))
      .statusCode(500)
      .body(containsString("Failed to delete user error due to:"));

    // Verify that the mock was called
    wireMockServer.verify(getRequestedFor(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching(".*userId.*" + userId + ".*")));

    // Clean up WireMock
    wireMockServer.resetRequests();
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

  @Test
  void cannotDeleteWithEmptyCQL() {
    usersClient.attemptToDeleteUsers(" ")
      .statusCode(400)
      .body(is("Expected CQL but query parameter is empty"));
  }

  @Test
  void createJPGProfilePictureInDb() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts"));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void createConfigWithMoreThan10MB() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
      .withMaxFileSize(11.0)).statusCode(500);
  }

  @Test
  void createConfigWithLessThan10MB() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
      .withMaxFileSize(9.9)).statusCode(204);
  }

  @Test
  void createConfigWithDifferentEncryptionKey() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG_1")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("aaaasrcgyihimbvgfrxyfjbytfrgjvdfd")
      .withMaxFileSize(6.8)).statusCode(400);
  }

  @Test
  void createConfigWithSameEncryptionKey() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG_1")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
      .withMaxFileSize(9.9)).statusCode(204);
  }

  @Test
  void createBigSizeJPGProfilePictureInDb() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts").withMaxFileSize(1.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("pexel.jpg");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void createBigSizeJPGProfilePictureInDbWithNoSize() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts"));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("pexel.jpg");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void createJPGProfilePictureInDbWithNullEncryptionKey() {
    configurationClient.updateConfiguration(new Config()
      .withConfigName("PROFILE_PICTURE_CONFIG")
      .withId(configurationClient.getConfigurationId())
      .withEnabled(true).withEnabledObjectStorage(false)
      .withEncryptionKey(null));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void createPNGProfilePictureInS3() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.png");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void ShouldNotCreateProfilePictureIfDisable() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(false).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.png");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void shouldNotCreateProfilePictureForUnknownTypes() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.heic");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void getProfilePictureFromDbTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    userProfilePictureClient.getUserProfilePicture(UUID.randomUUID().toString())
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void removeProfilePictureFromDbViaCleanUp() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    timerInterfaceClient.attemptToTriggerProfilePictureCleanUpProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void removeProfilePictureFromS3ViaCleanUp() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("isreeedfrgvbnmjhyuortidfhgjbnvtr")
        .withEnabled(true).withEnabledObjectStorage(true));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response1 = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);

    final var user = usersClient.createUser(User.builder()
      .username("julia123")
      .build());
    Personal p = Personal.builder().profilePictureLink(String.valueOf(response1.getId())).lastName("test").build();
    usersClient.attemptToUpdateUser(User.builder()
        .id(user.getId())
        .username("julia-brockhurst")
        .personal(p)
        .build())
      .statusCode(is(204));

    awaitUntilAsserted(() -> {
      final var updatedUser = usersClient.getUser(user.getId());
      assertThat(updatedUser.getPersonal().getProfilePictureLink(), is(response1.getId().toString()));
    });
    timerInterfaceClient.attemptToTriggerProfilePictureCleanUpProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
    userProfilePictureClient.getUserProfilePicture(response1.getId().toString())
      .statusCode(HTTP_OK);
  }

  @Test
  void cleanUpIfProfilePictureDisable() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEnabled(false).withEnabledObjectStorage(false));
    timerInterfaceClient.attemptToTriggerProfilePictureCleanUpProcess(TENANT_NAME)
      .statusCode(is(HTTP_OK));
  }

  @Test
  void getProfilePictureFromDbWithNullKeyTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey(null)
        .withEnabled(true).withEnabledObjectStorage(false));
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
  }

  @Test
  void getProfilePictureFromDbWithDisableTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(false).withEnabledObjectStorage(false));
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void updateProfilePicture() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_OK);
  }

  @Test
  void updateProfilePictureWithNullKey() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("isreeedfrgvbnmjhyuortidfhgjbnvtr")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey(null)
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_OK);
  }

  @Test
  void updateProfilePictureWithBigFile() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(1.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("pexel.jpg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void updateProfilePictureWithBigFileWhenNoSize() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("pexel.jpg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_OK);
  }

  @Test
  void updateProfilePictureDisableError() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(false).withEnabledObjectStorage(false));
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void updateProfilePictureInObjectStorage() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_OK);
  }

  @Test
  void updateProfilePictureErrorTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    //Trying to update profile picture with invalid id
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream1)
      .statusCode(HTTP_NOT_FOUND);

    //Trying to update profile picture with unsupported image
    InputStream inputStream2 = new ByteArrayInputStream(RandomStringUtils.randomAlphanumeric(100).getBytes());
    var response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream2)
      .statusCode(HTTP_INTERNAL_ERROR);
    System.out.println(response.extract().asString());
    assertThat(response.extract().asString(), is("Requested image should be of supported type-[PNG,JPG,JPEG]"));

    //Trying to update profile picture with insufficient data
    InputStream inputStream3 = new ByteArrayInputStream("1".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream3)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(),
      is("failed to save profile picture Insufficient data provided to detect file type"));

    //Trying to update profile picture with empty data
    InputStream inputStream4 = new ByteArrayInputStream("".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream4)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(),
      is("Requested file size should be within allowed size updated in profile_picture configuration"));

    //Trying to update profile picture with invalid uuid
    InputStream inputStream5 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture("1234", inputStream5)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void updateProfilePictureErrorTestInObjectStorage() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    //Trying to update profile picture with invalid id
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream1)
      .statusCode(HTTP_NOT_FOUND);

    //Trying to update profile picture with unsupported image
    InputStream inputStream2 = new ByteArrayInputStream(RandomStringUtils.randomAlphanumeric(100).getBytes());
    var response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream2)
      .statusCode(HTTP_INTERNAL_ERROR);
    System.out.println(response.extract().asString());
    assertThat(response.extract().asString(), is("Requested image should be of supported type-[PNG,JPG,JPEG]"));

    //Trying to update profile picture with insufficient data
    InputStream inputStream3 = new ByteArrayInputStream("1".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream3)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(),
      is("failed to save profile picture Insufficient data provided to detect file type"));

    //Trying to update profile picture with empty data
    InputStream inputStream4 = new ByteArrayInputStream("".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream4)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(),
      is("Requested file size should be within allowed size updated in profile_picture configuration"));

    //Trying to update profile picture with invalid uuid
    InputStream inputStream5 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture("1234", inputStream5)
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void deleteProfilePicture() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("isreeedfrgvbnmjhyuortidfhgjbnvtr")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    userProfilePictureClient.deleteUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_NO_CONTENT);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void deleteProfilePictureInObjectStorage() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEnabled(true).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    userProfilePictureClient.deleteUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_NO_CONTENT);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void deleteProfilePictureErrorInObjectStorage() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(false).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    userProfilePictureClient.deleteUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void deleteProfilePictureErrorTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(false).withMaxFileSize(5.0));
    //Trying to delete a profile picture which is not present
    var response = userProfilePictureClient.deleteUserProfilePicture(UUID.randomUUID().toString())
      .statusCode(HTTP_NOT_FOUND);
    assertThat(response.extract().asString(), is("Profile picture not found"));

    //Trying to delete a profile picture with invalid UUID
    userProfilePictureClient.deleteUserProfilePicture("1234")
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void deleteProfilePictureErrorInObjectStorageTest() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    //Trying to delete a profile picture which is not present
    var response = userProfilePictureClient.deleteUserProfilePicture(UUID.randomUUID().toString())
      .statusCode(HTTP_NOT_FOUND);
    assertThat(response.extract().asString(), is("Profile picture not found"));

    //Trying to delete a profile picture with invalid UUID
    userProfilePictureClient.deleteUserProfilePicture("1234")
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void getProfilePictureFromS3Test() {
    configurationClient.updateConfiguration(
      new Config().withConfigName("PROFILE_PICTURE_CONFIG").withId(configurationClient.getConfigurationId())
        .withEncryptionKey("ThisIsASimpleDefaultKeyToTestIts")
        .withEnabled(true).withEnabledObjectStorage(true).withMaxFileSize(5.0));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    userProfilePictureClient.getUserProfilePicture(UUID.randomUUID().toString())
      .statusCode(HTTP_NOT_FOUND);
  }

  User createUser(String username) {
    return usersClient.createUser(User.builder()
      .username(username)
      .build());
  }

  private void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
  }

  public static void assertDeleteEventForUser(User user) {
    final String userId = user.getId();
    await().until(() -> kafkaConsumer.getUsersEvents(userId).size(), greaterThan(0));
    assertDeleteEvent(kafkaConsumer.getLastUserEvent(userId));
  }

  public static void assertCreateEventForUser(User user) {
    final String userId = user.getId();
    await().until(() -> kafkaConsumer.getUsersEvents(userId).size(), greaterThan(0));
    assertCreateEvent(kafkaConsumer.getLastUserEvent(userId));
  }

  public static void assertUpdateEventForUser(User user) {
    final String userId = user.getId();
    await().until(() -> kafkaConsumer.getUsersEvents(userId).size(), greaterThan(0));
    assertUpdateEvent(kafkaConsumer.getLastUserEvent(userId));
  }
}
