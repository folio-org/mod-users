package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.util.UUID;

import org.folio.support.Group;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class GroupIT extends AbstractRestTestNoData {

  private static GroupsClient groupsClient;
  private static UsersClient usersClient;

  @BeforeAll
  public static void beforeAll() {
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
  }

  @Test
  void canCreateANewGroup() {
    var group = Group.builder()
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = groupsClient.createGroup(group);

    assertThat(createdGroup.getId(), is(notNullValue()));
    assertThat(createdGroup.getGroup(), is("New Group"));
    assertThat(createdGroup.getDesc(), is("Group description"));
    assertThat(createdGroup.getExpirationOffsetInDays(), is(365));
  }

  @Test
  void canUpdateAGroup() {
    var group = Group.builder()
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = groupsClient.createGroup(group);

    groupsClient.updateGroup(Group.builder()
      .id(createdGroup.getId())
      .group("A new name")
      .desc("A new description")
      .expirationOffsetInDays(365)
      .build());

    final var updatedGroup = groupsClient.getGroup(createdGroup.getId());

    assertThat(updatedGroup.getGroup(), is("A new name"));
    assertThat(updatedGroup.getDesc(), is("A new description"));
  }

  @Test
  void canDeleteAGroup() {
    final var group = groupsClient.createGroup(Group.builder()
      .group("New Group")
      .build());

    groupsClient.deleteGroup(group.getId());

    groupsClient.attemptToGetGroup(group.getId()).statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void cannotDeleteAGroupWithAssociatedUsers() {
    final var group = groupsClient.createGroup(Group.builder()
      .group("New Group")
      .build());

    usersClient.createUser(User.builder()
      .username("julia")
      .patronGroup(group.getId())
      .build());

    final var response = groupsClient.attemptToDeleteGroup(group.getId());

    response.statusCode(is(HTTP_BAD_REQUEST));
    response.body(is(
      String.format("Cannot delete groups.id = %s because id is still referenced from table users.",
        group.getId())));

    groupsClient.attemptToGetGroup(group.getId()).statusCode(HTTP_OK);
  }

  @Test
  void cannotDeleteAGroupThatDoesNotExist() {
    groupsClient.createGroup(Group.builder()
      .group("New Group")
      .build());

    final var response = groupsClient.attemptToDeleteGroup(UUID.randomUUID().toString());

    response.statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void cannotCreateAGroupWithTheSameNameAsExistingGroup() {
    groupsClient.createGroup(Group.builder()
      .group("New group")
      .build());

    final var response = groupsClient.attemptToCreateGroup(Group.builder()
      .group("New group")
      .build());

    response.statusCode(is(422));

    final var errors = response.extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("lower(f_unaccent(jsonb ->> 'group'::text)) value already exists in table groups: new group"));
  }

  @Test
  void canGetAGroup() {
    final var group = groupsClient.createGroup(Group.builder()
      .group("New group")
      .desc("Group description")
      .build());

    final var foundGroup = groupsClient.getGroup(group.getId());

    assertThat(foundGroup.getGroup(), is("New group"));
    assertThat(foundGroup.getDesc(), is("Group description"));
  }

  @Test
  void cannotGetAGroupThatDoesNotExist() {
    groupsClient.createGroup(Group.builder()
      .group("New group")
      .build());

    groupsClient.attemptToGetGroup(UUID.randomUUID().toString()).statusCode(HTTP_NOT_FOUND);
  }

  @Test
  void canGetAllGroups() {
    groupsClient.createGroup(Group.builder()
      .group("First new group")
      .desc("First group description")
      .build());

    groupsClient.createGroup(Group.builder()
      .group("Second new group")
      .desc("Second group description")
      .build());

    final var groups = groupsClient.getAllGroups();

    assertThat(groups.getTotalRecords(), is(2));

    final var firstGroup = groups.getGroupByName("First new group");

    assertThat("[First new group] exists in collection", firstGroup, is(notNullValue()));
    assertThat(firstGroup.getDesc(), is("First group description"));

    final var secondGroup = groups.getGroupByName("Second new group");

    assertThat("[Second new group] exists in collection", secondGroup, is(notNullValue()));
    assertThat(secondGroup.getDesc(), is("Second group description"));
  }

  @Test
  void canFindGroupByName() {
    groupsClient.createGroup(Group.builder()
      .group("First new group")
      .desc("First group description")
      .build());

    groupsClient.createGroup(Group.builder()
      .group("Second")
      .desc("Second group description")
      .build());

    final var groups = groupsClient.getGroups("group==Second");

    assertThat(groups.getTotalRecords(), is(1));

    final var secondGroup = groups.getGroupByName("Second");

    assertThat("[Second new group] exists in collection", secondGroup, is(notNullValue()));
    assertThat(secondGroup.getDesc(), is("Second group description"));
  }

  @Test
  void cannotCreateAUserForGroupThatDoesNotExist() {
    final var unknownGroupId = UUID.randomUUID().toString();

    final var response = usersClient.attemptToCreateUser(User.builder()
      .username("julia")
      .patronGroup(unknownGroupId)
      .build());

    response.statusCode(is(HTTP_BAD_REQUEST));
    response.body(is(
      String.format("Cannot add %s. Patron group not found", unknownGroupId)));
  }

  @Test
  void canAssignAGroupToAUser() {
    final var group = groupsClient.createGroup(Group.builder()
      .group("First new group")
      .build());

    final var user = usersClient.createUser(User.builder()
      .username("julia")
      .build());

    usersClient.updateUser(User.builder()
      .id(user.getId())
      .username("julia")
      .patronGroup(group.getId())
      .build());

    final var updatedUser = usersClient.getUser(user.getId());

    assertThat(updatedUser.getPatronGroup(), is(group.getId()));
  }

  @Test
  void cannotAssignGroupThatDoesNotExistToUser() {
    final var group = groupsClient.createGroup(Group.builder()
      .group("First new group")
      .build());

    final var user = usersClient.createUser(User.builder()
      .username("julia")
      .patronGroup(group.getId())
      .build());

    final var unknownGroupId = UUID.randomUUID().toString();

    final var response = usersClient.attemptToUpdateUser(User.builder()
      .id(user.getId())
      .username("julia")
      .patronGroup(unknownGroupId)
      .build());

    response.statusCode(is(HTTP_BAD_REQUEST));
    response.body(is(
      String.format("Cannot add %s. Patron group not found", unknownGroupId)));
  }

  @ParameterizedTest
  @CsvSource({"patronGroup.group/sort.ascending,julia", "patronGroup.group/sort.descending,alex"})
  void canSortUsersByPatronGroupNameAscending(String sortClause,
    String expectedFirstUsername) {

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

    final var usersSortedByGroup = usersClient.getUsers(
      "cql.allRecords=1 sortBy " + sortClause);

    assertThat(usersSortedByGroup.getTotalRecords(), is(2));
    assertThat(usersSortedByGroup.getFirstUser().getUsername(), is(expectedFirstUsername));
  }

  @Test
  void canFilterUsersByPatronGroup() {
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

    final var usersFilteredByGroupName = usersClient.getUsers(
      "patronGroup.group=alpha");

    assertThat(usersFilteredByGroupName.getTotalRecords(), is(1));
    assertThat(usersFilteredByGroupName.getFirstUser().getUsername(), is("julia"));
  }

  @Test
  void zeroUsersWhenFilteringUsersByPatronGroupThatDoesNotExist() {
    final var alphaGroup = groupsClient.createGroup(Group.builder()
      .group("Alpha group")
      .build());

    usersClient.createUser(User.builder()
      .username("julia")
      .patronGroup(alphaGroup.getId())
      .build());

    final var usersFilteredByGroupName = usersClient.getUsers(
      "patronGroup.group=missing");

    assertThat(usersFilteredByGroupName.getTotalRecords(), is(0));
  }
}
