package org.folio.rest.impl;

import java.lang.reflect.Field;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.folio.event.KafkaConfigSingleton;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, timeUnit = TimeUnit.SECONDS)
public class UsersNoKafkaTest {
  protected static VertxModule module;
  private static UsersClient usersClient;
  private static OkapiUrl okapiUrl;
  private static OkapiHeaders okapiHeaders;
  public static final String TENANT_NAME = "diku";

  @BeforeAll
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var port = NetworkUtils.nextFreePort();

    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);

    usersClient = new UsersClient(okapiUrl, okapiHeaders);

    module = new VertxModule(vertx);

    boolean hasData = false;

    try {
      KafkaConfigSingleton instance = KafkaConfigSingleton.INSTANCE;
      Field enabledField = KafkaConfigSingleton.class.getDeclaredField("enabled");
      enabledField.setAccessible(true);
      enabledField.setBoolean(instance, false);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      context.failNow(e);
      return;
    }

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, hasData, hasData))
      .onComplete(context.succeedingThenComplete());
  }

  @AfterAll
  static void after(VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .compose(v -> {
        PostgresClient.stopPostgresTester();
        return Future.succeededFuture();
      })
      .onComplete(context.succeedingThenComplete());
  }

  @BeforeEach
  void beforeEach() {
    usersClient.deleteAllUsers();
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
    final var personal = createdUser.getPersonal();

    assertThat(personal.getPronouns(), is("He/Him"));
    assertThat(personal.getLastName(), is("brockhurst"));
    assertThat(personal.getFirstName(), is("julia"));
    assertThat(personal.getPreferredFirstName(), is("jules"));

    assertThat(createdUser.getTags().getTagList(), containsInAnyOrder("foo", "bar"));
    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));
  }

}
