
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.UsersClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

// Loading the reference and sample data can take a long time
@Timeout(value = 30, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class ReferenceAndSampleDataIT {
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

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, tenant, token);

    usersClient = new UsersClient(okapiUrl.asURI(), headers);
    groupsClient = new GroupsClient(okapiUrl.asURI(), headers);
    addressTypesClient = new AddressTypesClient(okapiUrl, headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers, true, true))
      .onComplete(context.succeedingThenComplete());
  }

  @Test
  void defaultGroupsAreCreated() {
    final var allGroups = groupsClient.getAllGroups();

    assertThat(allGroups.getTotalRecords(), is(4));
    assertThat(allGroups.getNames(),
      containsInAnyOrder("graduate", "staff", "faculty", "undergrad"));
  }

  @Test
  void defaultAddressTypesAreCreated() {
    final var allAddressTypes = addressTypesClient.getAllAddressTypes();

    assertThat(allAddressTypes.getTotalRecords(), is(6));
    assertThat(allAddressTypes.getNames(),
      containsInAnyOrder("Home", "Claim", "Order", "Payment", "Returns",
        "Work"));
  }

  @Test
  void manySampleUsersAreCreated() {
    final var allUsers = usersClient.getAllUsers();

    assertThat(allUsers.getTotalRecords(), is(303));
  }
}
