
package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import org.apache.commons.lang3.RandomStringUtils;
import org.folio.moduserstest.AbstractRestTestNoData;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;
import io.vertx.core.Vertx;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.persist.PostgresClient;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.ConfigurationClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UserProfilePictureClient;
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
  private static UserProfilePictureClient userProfilePictureClient;
  private static ConfigurationClient configurationClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
    userTenantClient = new UserTenantClient(okapiUrl, okapiHeaders);
    userProfilePictureClient = new UserProfilePictureClient(okapiUrl, okapiHeaders);
    configurationClient = new ConfigurationClient(okapiUrl, okapiHeaders);
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
  void createJPGProfilePictureInDb() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void createPNGProfilePictureInS3() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.png");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void ShouldNotCreateProfilePictureIfDisable() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(false).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.png");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void shouldNotCreateProfilePictureForUnknownTypes() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(false));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.heic");
    userProfilePictureClient.saveUserProfilePicture(inputStream)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void getProfilePictureFromDbTest() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
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
  void updateProfilePicture() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
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
  void updateProfilePictureInObjectStorage() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
    InputStream inputStream = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    var response = userProfilePictureClient.saveUserProfilePicture(inputStream)
      .extract().as(ProfilePicture.class);
    userProfilePictureClient.getUserProfilePicture(response.getId().toString())
      .statusCode(HTTP_OK);
    InputStream inputStream1 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture(response.getId().toString(), inputStream1)
      .statusCode(HTTP_CREATED);
  }

  @Test
  void updateProfilePictureErrorTest() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(false));
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
    assertThat(response.extract().asString(), is("failed to save profile picture Insufficient data provided to detect file type"));

    //Trying to update profile picture with empty data
    InputStream inputStream4 = new ByteArrayInputStream("".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream4)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(), is("Requested file size should be within allowed size 0.1-10.0 megabytes"));

    //Trying to update profile picture with invalid uuid
    InputStream inputStream5 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture("1234", inputStream5)
      .statusCode(HTTP_INTERNAL_ERROR);
  }

  @Test
  void updateProfilePictureErrorTestInObjectStorage() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
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
    assertThat(response.extract().asString(), is("failed to save profile picture Insufficient data provided to detect file type"));

    //Trying to update profile picture with empty data
    InputStream inputStream4 = new ByteArrayInputStream("".getBytes());
    response = userProfilePictureClient.updateUserProfilePicture(UUID.randomUUID().toString(), inputStream4)
      .statusCode(HTTP_INTERNAL_ERROR);
    assertThat(response.extract().asString(), is("Requested file size should be within allowed size 0.1-10.0 megabytes"));

    //Trying to update profile picture with invalid uuid
    InputStream inputStream5 = getClass().getClassLoader().getResourceAsStream("sample.jpeg");
    userProfilePictureClient.updateUserProfilePicture("1234", inputStream5)
      .statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void deleteProfilePicture() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(false));
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
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
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
  void deleteProfilePictureErrorTest() {
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(false));
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
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
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
    configurationClient.updateConfiguration(new Config().withConfigName("PROFILE_PICTURE_CONFIG")
      .withEnabled(true).withEnabledObjectStorage(true));
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
}
