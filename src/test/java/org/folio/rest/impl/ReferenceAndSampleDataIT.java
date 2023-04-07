
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
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
import org.junit.jupiter.api.AfterAll;
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
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;
  private static EmbeddedKafkaCluster kafkaCluster;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var tenant = "referenceandsampledatait";
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    final var headers = new OkapiHeaders(okapiUrl, tenant, token);

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    usersClient = new UsersClient(okapiUrl, headers);
    groupsClient = new GroupsClient(okapiUrl, headers);
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

  @AfterAll
  public static void after() {
    kafkaCluster.stop();
  }
}
