
package org.folio.rest.impl;

import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.AddressInfo;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.GeneralInfo;
import org.folio.rest.jaxrs.model.PreferredEmailCommunication;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.support.http.StagingUsersClient;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.Collections;
import java.util.Set;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
    assertEquals(createdUser.getStatus(), stagingUserToCreate.getStatus());
    assertEquals(createdUser.getIsEmailVerified(), stagingUserToCreate.getIsEmailVerified());
    assertEquals(createdUser.getContactInfo(), stagingUserToCreate.getContactInfo());
    assertEquals(createdUser.getGeneralInfo(), stagingUserToCreate.getGeneralInfo());
    assertEquals(createdUser.getAddressInfo(), stagingUserToCreate.getAddressInfo());

    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));

    createdUser.setId(null);
    createdUser.getGeneralInfo().setFirstName("updated_firstname");
    createdUser.getAddressInfo().setCity("Updated_City");
    createdUser.getContactInfo().setMobilePhone("updated_12345");

    final var updatedNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    StagingUser updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertEquals(updatedUser.getContactInfo(), createdUser.getContactInfo());
    assertEquals(updatedUser.getGeneralInfo(), createdUser.getGeneralInfo());
    assertEquals(updatedUser.getAddressInfo(), createdUser.getAddressInfo());
    assertThat(updatedUser.getMetadata().getCreatedDate(), is(createdUser.getMetadata().getCreatedDate()));
    assertThat(updatedUser.getMetadata().getUpdatedDate(), not(createdUser.getMetadata().getUpdatedDate()));

  }

  @Test
  void validateStatusIsAlwaysFalseWhenNewStagingUserCreatedForTier1Case_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);
    // Setting status TIER_2
    stagingUserToCreate.setStatus(StagingUser.Status.TIER_2);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    //Validating status TIER_2 passed in request body is not considered and set TIER_1 on creation
    assertEquals(createdUser.getStatus(), StagingUser.Status.TIER_1);
  }

  @Test
  void validateIsEmailVerifiedIsAlwaysFalseWhenNewStagingUserCreatedForTier1Case_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);
    // Setting IsEmailVerified to true
    stagingUserToCreate.setIsEmailVerified(true);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    //Validating IsEmailVerified passed in request body is not considered and set false on creation
    assertFalse(createdUser.getIsEmailVerified());
  }

  @Test
  void shouldCreateAndUpdatePreferredEmailCommunicationCorrectlyInTheStagingUser_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    createdUser.setId(null);
    createdUser.setPreferredEmailCommunication(Collections.emptySet());

    var updatedNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    StagingUser updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertTrue(updatedUser.getPreferredEmailCommunication().containsAll(stagingUserToCreate.getPreferredEmailCommunication()));

    createdUser.setPreferredEmailCommunication(Set.of(PreferredEmailCommunication.PROGRAMS));
    updatedNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertEquals(1, updatedUser.getPreferredEmailCommunication().size());
    assertTrue(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.PROGRAMS));
    assertFalse(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.SERVICES));
    assertFalse(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.SUPPORT));
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

    stagingUserToCreate.setIsEmailVerified(false);
    stagingUserToCreate.setStatus(StagingUser.Status.TIER_1);
    stagingUserToCreate.setPreferredEmailCommunication(Set.of(PreferredEmailCommunication.PROGRAMS,
      PreferredEmailCommunication.SUPPORT, PreferredEmailCommunication.SERVICES));

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
