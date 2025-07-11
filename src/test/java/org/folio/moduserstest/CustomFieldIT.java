package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.RandomStringUtils;
import org.folio.support.CustomField;
import org.folio.support.PutCustomFieldsRequest;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.CustomFieldsClient;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
public class CustomFieldIT extends AbstractRestTestNoData {

  private static UsersClient usersClient;
  private static CustomFieldsClient customFieldsClient;

  @BeforeAll
  public static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    customFieldsClient = new CustomFieldsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    customFieldsClient.deleteAllCustomFields();
  }

  @Test
  void canCreateUserWithValueForCustomField() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    var createdCustomField = customFieldsClient.createCustomField(
      hobbiesCustomField(), maintainingUser);

    assertThat(createdCustomField.getDisplayInAccordion(), is("user_information"));

    final var createdUser = usersClient.attemptToCreateUser(User.builder()
        .username("some-user")
        .customFields(Map.of("hobbies", "cross-stitch"))
        .build())
      .statusCode(is(201))
      .extract().as(User.class);

    assertThat(createdUser.getCustomFields().size(), is(1));
    assertThat(createdUser.getCustomFields().get("hobbies"), is("cross-stitch"));

    final var fetchedUser = usersClient.getUser(createdUser.getId());

    assertThat(fetchedUser.getCustomFields().size(), is(1));
    assertThat(fetchedUser.getCustomFields().get("hobbies"), is("cross-stitch"));
  }

  @Test
  void canCreateAndDeleteCustomField() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    var createdCustomField = customFieldsClient.createCustomField(
      hobbiesCustomField(), maintainingUser);

    assertThat(createdCustomField.getDisplayInAccordion(), is("user_information"));
    var foundCustomFields = customFieldsClient.getCustomFields("cql.allRecords=1");
    assertThat(foundCustomFields.getTotalRecords(), is(1));

    customFieldsClient.deleteCustomField(createdCustomField.getId());
    var foundCustomFieldsAfterDelete = customFieldsClient.getCustomFields("cql.allRecords=1");
    assertThat(foundCustomFieldsAfterDelete.getTotalRecords(), is(0));
  }

  @Test
  void canCreateAndDeleteCustomFieldUsingUpdate() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    var customFieldToCreate = hobbiesCustomField();

    var bulkRequest = PutCustomFieldsRequest.builder()
      .customFields(List.of(customFieldToCreate))
      .entityType("user")
      .build();

    customFieldsClient.updateCustomFields(bulkRequest, maintainingUser);

    var foundCustomFields = customFieldsClient.getCustomFields("cql.allRecords=1");
    assertThat(foundCustomFields.getTotalRecords(), is(1));

    var emptyBulkRequest = PutCustomFieldsRequest.builder()
      .customFields(Collections.emptyList())
      .entityType("user")
      .build();

    customFieldsClient.updateCustomFields(emptyBulkRequest, maintainingUser);
    var foundCustomFieldsAfterUpdate = customFieldsClient.getCustomFields("cql.allRecords=1");
    assertThat(foundCustomFieldsAfterUpdate.getTotalRecords(), is(0));
  }

  @Test
  void cannotCreateUserWithCustomFieldThatDoesNotExist() {
    final var userToCreate = User.builder()
      .username("some-user")
      .customFields(Map.of("does-not-exist", "abc"));

    final var errors = usersClient.attemptToCreateUser(userToCreate
      .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));

    final var firstError = errors.getErrors().get(0);

    assertThat(firstError.getMessage(),
      is("Custom field with refId does-not-exist is not found"));

    assertThat(firstError.getParameters().get(0).getKey(), is("customFields"));
    assertThat(firstError.getParameters().get(0).getValue(), is("does-not-exist"));
  }

  @Test
  void cannotCreateCustomFieldWithInvalidValue() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    var customField = CustomField.builder()
      .name("Hobbies")
      .helpText("Describe hobbies")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .displayInAccordion("unknown-value")
      .build();

    customFieldsClient.attemptToCreateCustomField(customField, maintainingUser)
      .statusCode(is(422))
      .body("message", is("Display in accordion value must be one of: [user_information, " +
        "extended_information, contact_information, default, fees_fines, loans, requests]"));
  }

  @Test
  void cannotAssignCustomFieldThatDoesNotExistToAUser() {
    final var createdUser = usersClient.createUser(User.builder()
        .username("some-user")
      .build());

    final var userToUpdate = User.builder()
      .id(createdUser.getId())
      .username("some-user")
      .customFields(Map.of("does-not-exist", "abc"))
      .build();

    final var errors = usersClient.attemptToUpdateUser(userToUpdate)
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));

    final var firstError = errors.getErrors().get(0);

    assertThat(firstError.getMessage(),
      is("Custom field with refId does-not-exist is not found"));

    assertThat(firstError.getParameters().get(0).getKey(), is("customFields"));
    assertThat(firstError.getParameters().get(0).getValue(), is("does-not-exist"));
  }

  @Test
  void cannotCreateUserWithValueTooLongForCustomField() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    customFieldsClient.createCustomField(departmentCustomField(), maintainingUser);

    final var errors = usersClient.attemptToCreateUser(User.builder()
        .username("some-user")
        .customFields(Map.of("department", RandomStringUtils.randomAlphanumeric(151)))
        .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("Maximum length of the value is 150"));
  }

  @Test
  void canDeleteACustomField() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("some-user")
      .build());

    final var createdCustomField = customFieldsClient.createCustomField(
      departmentCustomField(), maintainingUser);

    customFieldsClient.attemptToDeleteCustomField(createdCustomField.getId())
      .statusCode(is(HTTP_NO_CONTENT));

    customFieldsClient.attemptToGetCustomField(createdCustomField.getId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void valuesAreRemovedWhenCustomFieldIsDeleted() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("maintaining-user")
      .build());

    final var createdCustomField = customFieldsClient.createCustomField(
      departmentCustomField(), maintainingUser);

    final var assignedUser = usersClient.createUser(User.builder()
      .username("some-user")
      .customFields(Map.of("department", "History"))
      .build());

    customFieldsClient.attemptToDeleteCustomField(createdCustomField.getId())
      .statusCode(is(HTTP_NO_CONTENT));

    final var updatedUser = usersClient.getUser(assignedUser.getId());

    assertThat(updatedUser.getCustomFields().size(), is(0));
  }

  @Test
  void customFieldCanBeUpdated() {
    final var creatingUser = usersClient.createUser(User.builder()
      .username("some-user")
      .build());

    final var createdCustomField = customFieldsClient.createCustomField(
      CustomField.builder()
        .name("Department")
        .visible(true)
        .required(true)
        .helpText("Provide a department")
        .entityType("user")
        .type("TEXTBOX_SHORT")
        .order(1)
        .build(), creatingUser);

    final var updatingUser = usersClient.createUser(User.builder()
      .username("some-other-user")
      .build());

    customFieldsClient.updateCustomField(CustomField.builder()
      .id(createdCustomField.getId())
      .name("Department updated")
      .visible(false)
      .required(true)
      .helpText("Provide a department")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .build(), updatingUser);

    final var fetchedCustomField = customFieldsClient.getCustomField(
      createdCustomField.getId());

    assertThat(fetchedCustomField.getName(), is("Department updated"));
    assertThat(fetchedCustomField.getVisible(), is(false));
  }

  @Test
  void canFindCustomFieldsByName() {
    final var creatingUser = usersClient.createUser(User.builder()
      .username("some-user")
      .build());

    final var departmentCustomField = customFieldsClient.createCustomField(
      CustomField.builder()
        .name("Department")
        .visible(true)
        .required(true)
        .helpText("Provide a department")
        .entityType("user")
        .type("TEXTBOX_SHORT")
        .order(1)
        .build(), creatingUser);

    customFieldsClient.createCustomField(
      CustomField.builder()
        .name("Hobbies")
        .visible(true)
        .required(true)
        .helpText("Describe user's hobbies")
        .entityType("user")
        .type("TEXTBOX_SHORT")
        .order(2)
        .build(), creatingUser);

    final var foundCustomFields = customFieldsClient.getCustomFields(
      "name=Department");

    assertThat(foundCustomFields.getTotalRecords(), is(1));
    assertThat(foundCustomFields.getCustomFields().get(0).getId(),
      is(departmentCustomField.getId()));
  }

  private static CustomField departmentCustomField() {
    return CustomField.builder()
      .name("Department")
      .helpText("Provide a department")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .build();
  }

  private static CustomField hobbiesCustomField() {
    return CustomField.builder()
      .name("Hobbies")
      .helpText("Describe hobbies")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .displayInAccordion("user_information")
      .build();
  }
}
