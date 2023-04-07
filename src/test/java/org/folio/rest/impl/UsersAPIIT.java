
package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.util.concurrent.TimeUnit.SECONDS;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import java.util.List;
import java.util.UUID;

import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class UsersAPIIT {
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var tenant = "usersapiit";
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl("http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, tenant, token);

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    usersClient = new UsersClient(okapiUrl, headers);
    groupsClient = new GroupsClient(okapiUrl, headers);
    addressTypesClient = new AddressTypesClient(okapiUrl, headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers))
      .onComplete(context.succeedingThenComplete());
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
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .build())
      .tags(TagList.builder().tagList(List.of("foo", "bar")).build())
      .build();

    final var createdUser = usersClient.createUser(userToCreate);

    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getUsername(), is("juliab"));
    assertThat(createdUser.getActive(), is(true));

    final var personal = createdUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));

    assertThat(createdUser.getTags().getTagList(),
      containsInAnyOrder("foo", "bar"));

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
    final var user = createUser("joannek");

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

  User createUser(String username) {
    return usersClient.createUser(User.builder()
      .username(username)
      .build());
  }

  private void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
  }

  @AfterAll
  public static void after() {
    kafkaCluster.stop();
  }
}
