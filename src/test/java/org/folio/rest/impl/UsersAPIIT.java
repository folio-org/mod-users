
package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import java.net.URI;
import java.util.List;
import java.util.UUID;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

/**
 * Most old UsersAPI tests are in deprecated org.folio.moduserstest.RestVerticleIT and
 * should be moved here.
 */
@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class UsersAPIIT {
  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var tenant = "users_integration_tests";
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var headers = new OkapiHeaders("http://localhost:" + port,
      tenant, token);

    usersClient = new UsersClient(new URI("http://localhost:" + port), headers);
    groupsClient = new GroupsClient(new URI("http://localhost:" + port), headers);
    addressTypesClient = new AddressTypesClient(
      new URI("http://localhost:" + port), headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .onComplete(context.succeeding(res -> module.enableModule(headers,
          false, false)
        .onComplete(context.succeedingThenComplete())));
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
    addressTypesClient.deleteAllAddressTypes();
  }

  @Test
  void canCreateUser() {
    final var homeAddressType = createAddressType("Home");
    final var returnsAddressType = createAddressType("Returns");

    final var userToCreate = User.builder()
      .username("juliab")
      .active(true)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(homeAddressType.getId()).build(),
          Address.builder().addressTypeId(returnsAddressType.getId()).build()))
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
    assertThat(personal.getAddresses().size(), is(2));

    assertThat(createdUser.getTags().getTagList(), containsInAnyOrder("foo", "bar"));

    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void canGetAUser() {
    final var homeAddressType = createAddressType("Home");

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

    assertThat(fetchedUser.getTags().getTagList(), containsInAnyOrder("foo", "bar"));

    assertThat(fetchedUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(fetchedUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

  @Test
  void cannotCreateUserWithMultipleAddressesOfSameType() {
    final var paymentAddressType = createAddressType("Payment");

    final var userWithMultipleAddresses = User.builder()
      .username("julia")
      .personal(Personal.builder()
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(paymentAddressType.getId()).build(),
          Address.builder().addressTypeId(paymentAddressType.getId()).build()))
        .build())
      .build();

    usersClient.attemptToCreateUser(userWithMultipleAddresses)
      .statusCode(400)
      .body(is("Users are limited to one address per addresstype"));
  }

  @Test
  void cannotCreateUserWithSameUsernameAsExistingUser() {
    usersClient.createUser("julia");

    final var response = usersClient.attemptToCreateUser(User.builder()
      .username("julia")
      .build());

    response.statusCode(is(422));

    final var errors = response.extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("User with this username already exists"));
  }

  @Test
  void cannotCreateUserWithSameIdAsExistingUser() {
    final var existingUser = usersClient.createUser("julia");

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
  void canGetPatronGroupFacetsForUsers() {
    final var alphaGroup = groupsClient.createGroup(Group.builder()
      .group("Alpha group")
      .build());

    var zebraGroup = groupsClient.createGroup(Group.builder()
      .group("Zebra group")
      .build());

    usersClient.createUser(User.builder()
      .username("julia")
      .patronGroup(alphaGroup.getId())
      .build());

    usersClient.createUser(User.builder()
      .username("alex")
      .patronGroup(zebraGroup.getId())
      .build());

    usersClient.createUser(User.builder()
      .username("steven")
      .patronGroup(zebraGroup.getId())
      .build());

    final var patronGroupFacets = usersClient.getPatronGroupFacets();

    assertThat(patronGroupFacets.getTotalRecords(), is(3));
    assertThat(patronGroupFacets.getFacetCount(zebraGroup.getId()), is(2));
    assertThat(patronGroupFacets.getFacetCount(alphaGroup.getId()), is(1));
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
    final var user = createUser("joannek");

    usersClient.attemptToDeleteUser(UUID.randomUUID().toString())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void canDeleteMultipleUsersUsingCQL() {
    final var user1 = createUser("1234");
    final var user2 = createUser("201");
    final var user3 = createUser("1999");

    deleteUsersByUsername("1*");

    userExists(user2.getId());
    usersClient.attemptToGetUser(user1.getId())
      .statusCode(404);
    usersClient.attemptToGetUser(user3.getId())
      .statusCode(404);
  }

  void userExists(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(200);
  }

  User createUser(String username) {
    return usersClient.createUser(User.builder()
      .username(username)
      .build());
  }

  private AddressType createAddressType(String Home) {
    return addressTypesClient.createAddressType(
      AddressType.builder()
        .addressType(Home)
        .build());
  }

  private void deleteUsersByUsername(String username) {
    usersClient.deleteUsers("username == \"" + username + "\"");
  }
}
