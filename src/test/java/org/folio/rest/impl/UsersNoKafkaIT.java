package org.folio.rest.impl;

import static org.folio.support.TestConstants.TENANT_NAME;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.folio.extensions.KafkaContainerExtension;
import org.folio.extensions.PostgresContainerExtension;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.VertxModule;
import org.folio.support.WireMockHelper;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.folio.support.tags.IntegrationTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@IntegrationTest
@Timeout(value = 20, timeUnit = TimeUnit.SECONDS)
@ExtendWith({ VertxExtension.class, PostgresContainerExtension.class })
class UsersNoKafkaIT {
  protected static VertxModule module;
  private static UsersClient usersClient;
  private static OkapiUrl okapiUrl;
  private static OkapiHeaders okapiHeaders;
  private static WireMockServer wireMockServer;
  private static WireMockHelper wireMockHelper;

  @BeforeAll
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    wireMockServer = new WireMockServer(new WireMockConfiguration().dynamicPort());
    wireMockServer.start();
    wireMockHelper = new WireMockHelper(wireMockServer);

    final var port = NetworkUtils.nextFreePort();
    final var token = new FakeTokenGenerator().generateToken();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    OkapiUrl wireMockUrl = new OkapiUrl("http://localhost:" + wireMockServer.port());
    okapiHeaders = new OkapiHeaders(wireMockUrl, TENANT_NAME, token);

    usersClient = new UsersClient(wireMockUrl, okapiHeaders);

    module = new VertxModule(vertx);
    KafkaContainerExtension.disableKafka(context);

    boolean hasData = false;

    wireMockHelper = new WireMockHelper(wireMockServer, okapiUrl.toString());
    wireMockHelper.mockConfiguration(); // otherwise POST /_/tenant fails during settings migration attempt

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
      .onComplete(ignored -> wireMockServer.stop())
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
