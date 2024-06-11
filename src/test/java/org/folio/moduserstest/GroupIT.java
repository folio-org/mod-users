package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.support.kafka.FakeKafkaConsumer.getLastUserGroupEvent;
import static org.folio.support.kafka.FakeKafkaConsumer.getUserGroupsEvents;
import static org.folio.support.kafka.FakeKafkaConsumer.removeAllEvents;
import static org.folio.support.matchers.DomainEventAssertions.assertBasicEventFields;
import static org.folio.support.matchers.DomainEventAssertions.assertHeaders;
import static org.folio.support.matchers.DomainEventAssertions.await;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;

import java.util.UUID;

import org.folio.service.event.DomainEventType;
import org.folio.support.Group;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UsersClient;
import org.folio.support.kafka.FakeKafkaConsumer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.kafka.client.consumer.KafkaConsumerRecord;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class GroupIT extends AbstractRestTestNoData {

  private static GroupsClient groupsClient;
  private static UsersClient usersClient;
  protected static FakeKafkaConsumer kafkaConsumer;

  @BeforeAll
  public static void before() {
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    kafkaConsumer = new FakeKafkaConsumer().consume(module.getVertx());
    removeAllEvents();
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    groupsClient.deleteAllGroups();
  }

  @Test
  void canCreateANewGroup() {
    String groupId = UUID.randomUUID().toString();
    var group = Group.builder()
      .id(groupId)
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = groupsClient.createGroup(group);
    await().until(() -> getUserGroupsEvents(groupId).size(), is(1));
    assertCreateEventForUserGroup(createdGroup);


    assertThat(createdGroup.getId(), is(notNullValue()));
    assertThat(createdGroup.getGroup(), is("New Group"));
    assertThat(createdGroup.getDesc(), is("Group description"));
    assertThat(createdGroup.getExpirationOffsetInDays(), is(365));
  }

  @Test
  void canUpdateAGroup() {
    String groupId = UUID.randomUUID().toString();
    var group = Group.builder()
      .id(groupId)
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = groupsClient.createGroup(group);
    await().until(() -> getUserGroupsEvents(groupId).size(), is(1));
    assertCreateEventForUserGroup(createdGroup);


    groupsClient.updateGroup(Group.builder()
      .id(createdGroup.getId())
      .group("A new name")
      .desc("A new description")
      .expirationOffsetInDays(365)
      .build());
    await().until(() -> getUserGroupsEvents(groupId).size(), is(2));
    assertUpdateEventForUserGroup(createdGroup);

    final var updatedGroup = groupsClient.getGroup(createdGroup.getId());

    assertThat(updatedGroup.getGroup(), is("A new name"));
    assertThat(updatedGroup.getDesc(), is("A new description"));
  }

  @Test
  void canDeleteAGroup() {
    String groupId = UUID.randomUUID().toString();
    final var group = groupsClient.createGroup(Group.builder()
      .id(groupId)
      .group("New Group")
      .build());
    await().until(() -> getUserGroupsEvents(groupId).size(), is(1));
    assertCreateEventForUserGroup(group);

    groupsClient.deleteGroup(group.getId());
    await().until(() -> getUserGroupsEvents(groupId).size(), is(2));
    assertDeleteEventForUserGroup(group);

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
    String firstGroupId = UUID.randomUUID().toString();
    Group firstCreatedGroup = groupsClient.createGroup(Group.builder()
      .id(firstGroupId)
      .group("First new group")
      .desc("First group description")
      .build());
    await().until(() -> getUserGroupsEvents(firstGroupId).size(), is(1));
    assertCreateEventForUserGroup(firstCreatedGroup);

    String secondGroupId = UUID.randomUUID().toString();
    Group secondCreatedGroup = groupsClient.createGroup(Group.builder()
      .id(secondGroupId)
      .group("Second new group")
      .desc("Second group description")
      .build());
    await().until(() -> getUserGroupsEvents(secondGroupId).size(), is(1));
    assertCreateEventForUserGroup(secondCreatedGroup);

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

  public static void assertCreateEventForUserGroup(Group userGroup) {
    final String userGroupId = userGroup.getId();

    await().until(() -> getUserGroupsEvents(userGroupId).size(), greaterThan(0));

    assertCreateEvent(getLastUserGroupEvent(userGroupId));
  }

  public static void assertUpdateEventForUserGroup(Group userGroup) {
    final String userGroupId = userGroup.getId();

    await().until(() -> getUserGroupsEvents(userGroupId).size(), greaterThan(0));

    assertUpdateEvent(getLastUserGroupEvent(userGroupId));
  }

  public static void assertDeleteEventForUserGroup(Group userGroup) {
    final String userGroupId = userGroup.getId();

    await().until(() -> getUserGroupsEvents(userGroupId).size(), greaterThan(0));

    assertDeleteEvent(getLastUserGroupEvent(userGroupId));
  }

  private static void assertCreateEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Create event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.CREATED);
    assertHeaders(createEvent.headers());
  }

  private static void assertUpdateEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Update event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.UPDATED);
    assertHeaders(createEvent.headers());
  }

  private static void assertDeleteEvent(KafkaConsumerRecord<String, JsonObject> createEvent) {
    assertThat("Delete event should be present", createEvent.value(), is(notNullValue()));
    assertBasicEventFields(createEvent, DomainEventType.DELETED);
    assertHeaders(createEvent.headers());
  }
}
