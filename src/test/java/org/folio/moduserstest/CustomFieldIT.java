package org.folio.moduserstest;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Map;

import org.apache.commons.lang3.RandomStringUtils;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.CustomField;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
import org.folio.support.http.CustomFieldsClient;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
public class CustomFieldIT {
  private static final String joeBlockId = "ba6baf95-bf14-4020-b44c-0cad269fb5c9";
  private static final String johnRectangleId = "ae6d1c57-3041-4645-9215-3ca0094b77fc";
  private static final String notExistingCustomField = "notExistingCustomField";
  private static final String customFieldId = "524d3210-9ca2-4f91-87b4-d2227d595aaa";

  private static UsersClient usersClient;
  private static CustomFieldsClient customFieldsClient;

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var tenant = "diku";
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl("http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, tenant, token);

    usersClient = new UsersClient(okapiUrl, headers);
    customFieldsClient = new CustomFieldsClient(okapiUrl, headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers))
      .onComplete(context.succeedingThenComplete());
  }

  @BeforeEach
  public void beforeEach() {
    usersClient.deleteAllUsers();
    customFieldsClient.deleteAllCustomFields();
  }

  @Order(1)
  @Test
  void test1Sequential() {
    postUser()
      .compose(v -> postCustomField())
      .compose(v -> postUserWithInvalidCustomFieldValueLength())
      .compose(v -> postUserWithCustomFields())
      .compose(v -> getUserWithCustomFields())
      .compose(v -> deleteUser(johnRectangleId))
      .compose(v -> putUserWithNotExistingCustomField())
      .compose(v -> postUserWithNotExistingCustomField())
      .compose(v -> deleteUser(joeBlockId))
      .compose(v -> deleteCustomField());
  }

  @Order(2)
  @Test
  void test4CustomFields() {
    postUser()
      .compose(v -> postCustomField())
      .compose(v -> putCustomField())
      .compose(v -> queryCustomField())
      .compose(v -> deleteUser(joeBlockId))
      .compose(v -> deleteCustomField());
  }

  private Future<Void> postUser() {
    final var userToCreate = User.builder()
      .id(joeBlockId)
      .username("joeBlock")
      .active(true)
      .build();

    usersClient.createUser(userToCreate);

    return Future.succeededFuture();
  }

  private Future<Void> postUserWithCustomFields() {
    final var userToCreate = User.builder()
      .id(johnRectangleId)
      .username("johnRectangle")
      .active(true)
      .customFields(Map.of("department", "Math"))
      .build();

    usersClient.createUser(userToCreate);

    return Future.succeededFuture();
  }

  private Future<Void> postUserWithInvalidCustomFieldValueLength() {
    final var userToCreate = User.builder()
      .id(johnRectangleId)
      .username("johnRectangle")
      .active(true)
      .customFields(Map.of("department", RandomStringUtils.randomAlphanumeric(151)))
      .build();

    final var errors = usersClient.attemptToCreateUser(userToCreate)
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("Maximum length of the value is 150"));

    return Future.succeededFuture();
  }

  private Future<Void> getUserWithCustomFields() {
    final var user = usersClient.getUser(johnRectangleId);

    assertThat(user.getCustomFields().size(), is(1));
    assertThat(user.getCustomFields().get("department"), is("Math"));

    return Future.succeededFuture();
  }

  private Future<Void> putUserWithNotExistingCustomField() {
    final var userToCreate = User.builder()
      .id(johnRectangleId)
      .username("johnRectangle")
      .active(true)
      .customFields(Map.of(notExistingCustomField, "abc"))
      .build();

    final var errors = usersClient.attemptToUpdateUser(userToCreate)
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));
    final var firstError = errors.getErrors().get(0);

    assertThat(firstError.getMessage(),
      is("Custom field with refId notExistingCustomField is not found"));
    assertThat(firstError.getParameters().get(0).getKey(), is("customFields"));
    assertThat(firstError.getParameters().get(0).getValue(), is(notExistingCustomField));

    return Future.succeededFuture();
  }

  private Future<Void> postUserWithNotExistingCustomField() {
    final var userToCreate = User.builder()
      .id(johnRectangleId)
      .username("johnRectangle")
      .active(true)
      .customFields(Map.of(notExistingCustomField, "abc"))
      .build();

    final var errors = usersClient.attemptToCreateUser(userToCreate)
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().size(), is(1));
    final var firstError = errors.getErrors().get(0);

    assertThat(firstError.getMessage(),
      is("Custom field with refId notExistingCustomField is not found"));

    assertThat(firstError.getParameters().get(0).getKey(), is("customFields"));
    assertThat(firstError.getParameters().get(0).getValue(), is(notExistingCustomField));

    return Future.succeededFuture();
  }

  private Future<Void> deleteCustomField() {
    customFieldsClient.deleteCustomField(customFieldId);

    return Future.succeededFuture();
  }

  private Future<Void> queryCustomField() {
    final var customFields = customFieldsClient.getCustomFields(
      "entityType==user");

    assertThat(customFields.getTotalRecords(), is(1));
    assertThat(customFields.getCustomFields().get(0).getEntityType(), is("user"));

    return Future.succeededFuture();
  }

  private Future<Void> postCustomField() {
    final var creatingUser = usersClient.createUser(User.builder()
      .username("some-user")
      .build());

    customFieldsClient.createCustomField(CustomField.builder()
      .id(customFieldId)
      .name("Department")
      .visible(true)
      .required(true)
      .helpText("Provide a department")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .build(), creatingUser);

    return Future.succeededFuture();
  }

  private Future<Void> putCustomField() {
    final var updatingUser = usersClient.createUser(User.builder()
      .username("some-other-user")
      .build());

    customFieldsClient.updateCustomField(CustomField.builder()
      .id(customFieldId)
      .name("Department updated")
      .visible(false)
      .required(true)
      .helpText("Provide a department")
      .entityType("user")
      .type("TEXTBOX_SHORT")
      .order(1)
      .build(), updatingUser);

    return Future.succeededFuture();
  }

  private Future<Void> deleteUser(String userId) {
    usersClient.deleteUser(userId);

    return Future.succeededFuture();
  }
}
