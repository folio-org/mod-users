
package org.folio.rest.impl;

import static org.folio.rest.utils.ManualBlockWiremockStubs.addManualBlockStubForDeleteUserById;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.HashMap;
import java.util.Map;

import io.vertx.core.Future;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.folio.moduserstest.AbstractRestTestWithData;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UsersClient;
import org.folio.support.tags.IntegrationTest;
import lombok.SneakyThrows;

@IntegrationTest
class ReferenceAndSampleDataMigrationIT extends AbstractRestTestWithData {

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
  }

  @Test
  void testMigration(VertxTestContext vtc) {
    addManualBlockStubForDeleteUserById(wireMockServer);
    // Create custom Okapi headers with WireMock base URL for the delete operation
    Map<String, String> customHeaders = new HashMap<>();
    customHeaders.put("X-Okapi-Url",  "http://localhost:" + wireMockServer.port());
    usersClient.deleteUser("ab579dc3-219b-4f5b-8068-ab1c7a55c402", customHeaders); // users-15.4.0/User001.json
    usersClient.deleteUser("bec20636-fb68-41fd-84ea-2cf910673599" ,customHeaders); // users-17.3.0/User301.json
    migrate("17.3.0", 5, 6, 301)
    .compose(x -> migrate("15.4.0", 5, 6, 302))
    .compose(x -> migrate("15.3.9", 5, 6, 303))
    .onComplete(vtc.succeedingThenComplete());

    // Clean up WireMock
    wireMockServer.resetRequests();
  }

  Future<Void> migrate(String fromVersion, int groups, int addressTypes, int users) {
    return module.migrateModule(okapiHeaders, fromVersion)
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

}
