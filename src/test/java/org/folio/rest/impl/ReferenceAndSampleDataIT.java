
package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.folio.moduserstest.AbstractRestTestWithData;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.UsersClient;
import org.folio.support.tags.IntegrationTest;

// Loading the reference and sample data can take a long time
@IntegrationTest
class ReferenceAndSampleDataIT extends AbstractRestTestWithData {

  private static UsersClient usersClient;
  private static GroupsClient groupsClient;
  private static AddressTypesClient addressTypesClient;

  @BeforeAll
  static void beforeAll() {
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
  }

  @Test
  void defaultGroupsAreCreated() {
    final var allGroups = groupsClient.getAllGroups();

    assertThat(allGroups.getTotalRecords(), is(5));
    assertThat(allGroups.getNames(),
      containsInAnyOrder("graduate", "staff", "faculty", "undergrad", "Remote Non-circulating"));
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
