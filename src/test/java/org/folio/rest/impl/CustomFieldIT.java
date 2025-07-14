package org.folio.rest.impl;

import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.rest.jaxrs.model.CustomField.Type.TEXTBOX_SHORT;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.jupiter.api.Test;

import org.folio.rest.jaxrs.model.CustomField;
import org.folio.rest.jaxrs.model.PutCustomFieldCollection;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.tags.IntegrationTest;

@IntegrationTest
class CustomFieldIT extends CustomFieldTestBase {

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
      .statusCode(SC_CREATED)
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

    var bulkRequest = new PutCustomFieldCollection()
      .withCustomFields(List.of(customFieldToCreate))
      .withEntityType("user");

    customFieldsClient.updateCustomFields(bulkRequest, maintainingUser);

    var foundCustomFields = customFieldsClient.getCustomFields("cql.allRecords=1");
    assertThat(foundCustomFields.getTotalRecords(), is(1));

    var emptyBulkRequest = new PutCustomFieldCollection().withEntityType("user");

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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));

    final var firstError = errors.getErrors().getFirst();

    assertThat(firstError.getMessage(),
      is("Custom field with refId does-not-exist is not found"));

    assertThat(firstError.getParameters().getFirst().getKey(), is("customFields"));
    assertThat(firstError.getParameters().getFirst().getValue(), is("does-not-exist"));
  }

  @Test
  void cannotCreateCustomFieldWithInvalidValue() {
    final var maintainingUser = usersClient.createUser(User.builder()
      .username("admin-user")
      .build());

    var customField = new CustomField()
      .withName("Hobbies")
      .withHelpText("Describe hobbies")
      .withEntityType("user")
      .withType(TEXTBOX_SHORT)
      .withOrder(1)
      .withDisplayInAccordion("unknown-value");

    customFieldsClient.attemptToCreateCustomField(customField, maintainingUser)
      .statusCode(SC_UNPROCESSABLE_ENTITY)
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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));

    final var firstError = errors.getErrors().getFirst();

    assertThat(firstError.getMessage(),
      is("Custom field with refId does-not-exist is not found"));

    assertThat(firstError.getParameters().getFirst().getKey(), is("customFields"));
    assertThat(firstError.getParameters().getFirst().getValue(), is("does-not-exist"));
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
      .statusCode(SC_UNPROCESSABLE_ENTITY)
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().getFirst().getMessage(),
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
      new CustomField()
        .withName("Department")
        .withVisible(true)
        .withRequired(true)
        .withHelpText("Provide a department")
        .withEntityType("user")
        .withType(TEXTBOX_SHORT)
        .withOrder(1), creatingUser);

    final var updatingUser = usersClient.createUser(User.builder()
      .username("some-other-user")
      .build());

    customFieldsClient.updateCustomField(new CustomField()
      .withId(createdCustomField.getId())
      .withName("Department updated")
      .withVisible(false)
      .withRequired(true)
      .withHelpText("Provide a department")
      .withEntityType("user")
      .withType(TEXTBOX_SHORT)
      .withOrder(1), updatingUser);

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
      new CustomField()
        .withName("Department")
        .withVisible(true)
        .withRequired(true)
        .withHelpText("Provide a department")
        .withEntityType("user")
        .withType(TEXTBOX_SHORT)
        .withOrder(1), creatingUser);

    customFieldsClient.createCustomField(
      new CustomField()
        .withName("Hobbies")
        .withVisible(true)
        .withRequired(true)
        .withHelpText("Describe user's hobbies")
        .withEntityType("user")
        .withType(TEXTBOX_SHORT)
        .withOrder(2), creatingUser);

    final var foundCustomFields = customFieldsClient.getCustomFields(
      "name=Department");

    assertThat(foundCustomFields.getTotalRecords(), is(1));
    assertThat(foundCustomFields.getCustomFields().getFirst().getId(),
      is(departmentCustomField.getId()));
  }

  private static CustomField departmentCustomField() {
    return new CustomField()
      .withName("Department")
      .withHelpText("Provide a department")
      .withEntityType("user")
      .withType(TEXTBOX_SHORT)
      .withOrder(1);
  }

  private static CustomField hobbiesCustomField() {
    return new CustomField()
      .withName("Hobbies")
      .withHelpText("Describe hobbies")
      .withEntityType("user")
      .withType(TEXTBOX_SHORT)
      .withOrder(1)
      .withDisplayInAccordion("user_information");
  }
}
