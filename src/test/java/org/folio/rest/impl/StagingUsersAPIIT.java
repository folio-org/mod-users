
package org.folio.rest.impl;

import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.AddressInfo;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.GeneralInfo;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.support.http.StagingUsersClient;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class StagingUsersAPIIT extends AbstractRestTestNoData {

  private static StagingUsersClient stagingUsersClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    stagingUsersClient = new StagingUsersClient(okapiUrl, okapiHeaders);
  }

  @Test
  void shouldCreateAndUpdateTheStagingUser_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getContactInfo().getEmail(), is(stagingUserToCreate.getContactInfo().getEmail()));
    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));


    // Validating updating works as expected
    createdUser.setId(null);
    createdUser.getGeneralInfo().setFirstName("updated_firstname");
    createdUser.getAddressInfo().setCity("Updated_City");
    createdUser.getContactInfo().setMobilePhone("updated_12345");

    final var updatedNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    StagingUser updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertThat(updatedUser.getGeneralInfo().getFirstName(), is(createdUser.getGeneralInfo().getFirstName()));
    assertThat(updatedUser.getAddressInfo().getCity(), is(createdUser.getAddressInfo().getCity()));
    assertThat(updatedUser.getContactInfo().getMobilePhone(), is(createdUser.getContactInfo().getMobilePhone()));
    assertThat(updatedUser.getMetadata().getCreatedDate(), is(createdUser.getMetadata().getCreatedDate()));
    assertThat(updatedUser.getMetadata().getUpdatedDate(), not(createdUser.getMetadata().getUpdatedDate()));

  }

  @Test
  void shouldCreateAndGetTheStagingUserByCQL_success() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);


    final var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("contactInfo.email=="+createdUser.getContactInfo().getEmail());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);

    assertNotNull(stagingUserdataCollection.getStagingUsers());
    assertFalse(stagingUserdataCollection.getStagingUsers().isEmpty());
    assertThat(stagingUserdataCollection.getTotalRecords(), is(1));
    StagingUser stagingUser = stagingUserdataCollection.getStagingUsers().get(0);
    assertThat(stagingUser.getGeneralInfo().getFirstName(), is(createdUser.getGeneralInfo().getFirstName()));
    assertThat(stagingUser.getAddressInfo().getCity(), is(createdUser.getAddressInfo().getCity()));
    assertThat(stagingUser.getContactInfo().getMobilePhone(), is(createdUser.getContactInfo().getMobilePhone()));
    assertThat(stagingUser.getIsEmailVerified(), is(createdUser.getIsEmailVerified()));
    assertThat(stagingUser.getStatus(), is(createdUser.getStatus()));
    assertThat(stagingUser.getMetadata().getCreatedDate(), is(createdUser.getMetadata().getCreatedDate()));
    assertThat(stagingUser.getMetadata().getUpdatedDate(), is(createdUser.getMetadata().getUpdatedDate()));
    assertThat(stagingUser.getMetadata().getCreatedByUserId(), is(createdUser.getMetadata().getCreatedByUserId()));

  }

  @Test
  void cannotSearchUsingInvalidCQL() {
    stagingUsersClient.attemptToGetUsers("username==")
      .statusCode(is(HTTP_BAD_REQUEST))
      .body(containsString("expected index or term, got EOF"));
  }

  @NotNull
  private static StagingUser getDummyStagingUser(String randomString) {
    StagingUser stagingUserToCreate = new StagingUser();
    GeneralInfo generalInfo = new GeneralInfo();
    generalInfo.setFirstName("Kapil_" + randomString);
    generalInfo.setLastName("Soni_" + randomString);
    generalInfo.setPreferredFirstName("dummy_PreferredFirstName_" + randomString);
    generalInfo.setMiddleName("dummy_MiddleName_" + randomString);
    stagingUserToCreate.setGeneralInfo(generalInfo);

    ContactInfo contactInfo = new ContactInfo();
    contactInfo.setMobilePhone(randomString);
    contactInfo.setPhone(randomString);
    contactInfo.setEmail("kapilsoni@test_folio.com" + randomString);
    stagingUserToCreate.setContactInfo(contactInfo);

    AddressInfo addressInfo = new AddressInfo();
    addressInfo.setAddressLine0("dummy_AddressLine0_" + randomString);
    addressInfo.setAddressLine1("dummy_AddressLine1_" + randomString);
    addressInfo.setCity("dummy_City_" + randomString);
    addressInfo.setProvince("dummy_province_" + randomString);
    addressInfo.setCountry("dummy_country_" + randomString);
    addressInfo.setZip("dummy_123456_" + randomString);
    stagingUserToCreate.setAddressInfo(addressInfo);
    return stagingUserToCreate;
  }
}
