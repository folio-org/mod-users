
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.net.URI;
import java.util.List;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Address;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
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
  static final String HOME_ADDRESS_TYPE_ID = "93d3d88d-499b-45d0-9bc7-ac73c3a19880";
  static final String ClAIM_ADDRESS_TYPE_ID = "b6f4d1c6-0dfa-463c-9534-f49c4f0ae090";
  private static UsersClient usersClient;
  private static GroupsClient groupsClient;

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

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .onComplete(context.succeeding(res -> module.enableModule(headers,
          true, false)
        .onComplete(context.succeedingThenComplete())));
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
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
      .patronGroup(alphaGroup.getId()).build());

    usersClient.createUser(User.builder()
      .username("alex")
      .patronGroup(zebraGroup.getId()).build());

    usersClient.createUser(User.builder()
      .username("steven")
      .patronGroup(zebraGroup.getId()).build());

    final var patronGroupFacets = usersClient.getPatronGroupFacets();

    assertThat(patronGroupFacets.getTotalRecords(), is(3));
    assertThat(patronGroupFacets.getFacetCount(zebraGroup.getId()), is(2));
    assertThat(patronGroupFacets.getFacetCount(alphaGroup.getId()), is(1));
  }

  @Test
  void deleteMultipleUsersUsingCQL() {
    final var user1 = createUser("1234");
    final var user2 = createUser("201");
    final var user3 = createUser("1999");

    deleteUsersByUsername("1*");

    userExists(user2.getId());
    userDoesntExist(user1.getId());
    userDoesntExist(user3.getId());
  }

  @Test
  void canCreateUser() {
    final var userToCreate = User.builder()
      .username("julia")
      .personal(Personal.builder()
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build(),
          Address.builder().addressTypeId(ClAIM_ADDRESS_TYPE_ID).build()))
        .build())
      .build();

    final var createdUser = usersClient.createUser(userToCreate);

    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getUsername(), is("julia"));

    final var personal = createdUser.getPersonal();

    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getAddresses().size(), is(2));
  }

  @Test
  void cannotCreateUserWithMultipleAddressesOfSameType() {
    final var userWithMultipleAddresses = User.builder()
      .username("julia")
      .personal(Personal.builder()
        .lastName("brockhurst")
        .addresses(List.of(
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build(),
          Address.builder().addressTypeId(HOME_ADDRESS_TYPE_ID).build()))
        .build())
      .build();

    usersClient.attemptToCreateUser(userWithMultipleAddresses)
      .statusCode(400)
      .body(is("Users are limited to one address per addresstype"));
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
  void cannotCreateUserWithSameUsernameAsExistingUser() {
    usersClient.createUser("julia");

    final var response = usersClient.attemptToCreateUser(User.builder()
      .username("julia").build());

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

  void userExists(String id) {
    usersClient.attemptToGetUser(id)
      .statusCode(200);
  }

  void userDoesntExist(String id) {
    usersClient.attemptToGetUser(id)
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
}
