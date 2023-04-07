
package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.SECONDS;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

@Timeout(value = 30, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class ReferenceAndSampleDataMigrationIT {
  private static final String KAFKA_ENV_VALUE = "test-env";
  private static final String KAFKA_HOST = "KAFKA_HOST";
  private static final String KAFKA_PORT = "KAFKA_PORT";
  private static final String KAFKA_ENV = "ENV";

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;
  private static EmbeddedKafkaCluster kafkaCluster;
  private static OkapiHeaders headers;
  private static VertxModule module;

  @BeforeAll
  @SneakyThrows
  static void beforeAll(Vertx vertx, VertxTestContext context) {
    final var tenant = "datamigrationit";
    final var token = new FakeTokenGenerator().generateToken();

    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    final var port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    headers = new OkapiHeaders(okapiUrl, tenant, token);

    kafkaCluster = EmbeddedKafkaCluster.provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");
    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);

    usersClient = new UsersClient(okapiUrl, headers);
    groupsClient = new GroupsClient(okapiUrl, headers);
    addressTypesClient = new AddressTypesClient(okapiUrl, headers);

    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(x -> module.enableModule(headers, true, true))
      .onComplete(context.succeedingThenComplete());
  }

  @Test
  void testMigration(VertxTestContext vtc) {
    usersClient.deleteUser("ab579dc3-219b-4f5b-8068-ab1c7a55c402"); // users-15.4.0/User001.json
    usersClient.deleteUser("bec20636-fb68-41fd-84ea-2cf910673599"); // users-17.3.0/User301.json
    migrate("17.3.0", 4, 6, 301)
    .compose(x -> migrate("15.4.0", 4, 6, 302))
    .compose(x -> migrate("15.3.9", 4, 6, 303))
    .onComplete(vtc.succeedingThenComplete());
  }

  Future<Void> migrate(String fromVersion, int groups, int addressTypes, int users) {
    return module.migrateModule(headers, fromVersion)
        .map(x -> {
          assertData(fromVersion, groups, addressTypes, users);
          return null;
        });
  }

  void assertData(String fromVersion, int groups, int addressTypes, int users) {
    assertThat(fromVersion + " groups",
        groupsClient.getAllGroups().getUsergroups().size(), is(groups));
    assertThat(fromVersion + " addressTypes",
        addressTypesClient.getAllAddressTypes().getAddressTypes().size(), is(addressTypes));
    assertThat(fromVersion + " users",
        usersClient.getAllUsers().getUsers().size(), is(users));
  }

  @AfterAll
  public static void after() {
    kafkaCluster.stop();
  }
}
