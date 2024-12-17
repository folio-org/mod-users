
package org.folio.rest.impl;

import io.vertx.core.Vertx;
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
import org.folio.rest.jaxrs.model.StagingUserMergeResponse;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Group;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.StagingUsersClient;
import org.folio.support.http.UsersClient;
import org.folio.test.util.DBTestUtil;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.Collections;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.containsString;
import static org.folio.service.impl.StagingUserService.CONTACT_TYPE_EMAIL_ID;
import static org.folio.service.impl.StagingUserService.HOME;
import static org.folio.service.impl.StagingUserService.REMOTE_NON_CIRCULATING;
import static org.folio.service.impl.StagingUserService.STAGING_USER_NOT_FOUND;
import static org.folio.service.impl.StagingUserService.USER_NOT_FOUND;


import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class StagingUsersAPIIT extends AbstractRestTestNoData {

  private static StagingUsersClient stagingUsersClient;
  private static UsersClient usersClient;
  private static AddressTypesClient addressTypesClient;
  private static GroupsClient groupsClient;

  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    stagingUsersClient = new StagingUsersClient(okapiUrl, okapiHeaders);
    usersClient = new UsersClient(okapiUrl, okapiHeaders);
    addressTypesClient = new AddressTypesClient(okapiUrl, okapiHeaders);
    groupsClient = new GroupsClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach(Vertx vertx) {
    deleteAllStagingUsers(vertx);
    usersClient.deleteAllUsers();
    addressTypesClient.deleteAllAddressTypes();
    groupsClient.deleteAllGroups();
  }

  @Test
  void validateStatusAndIsEmailVerifiedIsSetWhenPassNonNull_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);

    stagingUserToCreate.setStatus(StagingUser.Status.TIER_1);
    stagingUserToCreate.setIsEmailVerified(true);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertEquals(StagingUser.Status.TIER_1, createdUser.getStatus());
    assertNotNull(createdUser.getExternalSystemId());
    assertTrue(createdUser.getIsEmailVerified());
  }

  @Test
  void validateStatusAndIsEmailVerifiedIsSetWhenPassNull_positive() {
    String randomString = RandomStringUtils.random(5, true, true);
    StagingUser stagingUserToCreate = getDummyStagingUser(randomString);

    stagingUserToCreate.setStatus(null);
    stagingUserToCreate.setIsEmailVerified(null);
    final var createdNewStagingUserResponse = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    createdNewStagingUserResponse.statusCode(is(201));
    StagingUser createdUser = createdNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertEquals(StagingUser.Status.TIER_1, createdUser.getStatus());
    assertNotNull(createdUser.getExternalSystemId());
    assertFalse(createdUser.getIsEmailVerified());
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

    assertThat(stagingUser.getId(), is(notNullValue()));
    assertEquals(createdUser.getStatus(), stagingUser.getStatus());
    assertTrue(stagingUser.getPreferredEmailCommunication().containsAll(createdUser.getPreferredEmailCommunication()));
    assertEquals(createdUser.getIsEmailVerified(), stagingUser.getIsEmailVerified());
    assertEquals(createdUser.getContactInfo(), stagingUser.getContactInfo());
    assertEquals(createdUser.getGeneralInfo(), stagingUser.getGeneralInfo());
    assertEquals(createdUser.getAddressInfo(), stagingUser.getAddressInfo());
    assertThat(stagingUser.getMetadata().getCreatedDate(), is(createdUser.getMetadata().getCreatedDate()));
    assertThat(stagingUser.getMetadata().getUpdatedDate(), is(createdUser.getMetadata().getUpdatedDate()));
    assertThat(stagingUser.getMetadata().getCreatedByUserId(), is(createdUser.getMetadata().getCreatedByUserId()));
    assertNotNull(stagingUser.getExternalSystemId());
  }

  @Test
  void cannotSearchUsingInvalidCQL() {
    stagingUsersClient.attemptToGetUsers("username==")
      .statusCode(is(HTTP_BAD_REQUEST))
      .body(containsString("expected index or term, got EOF"));
  }

  @Test
  void testMergeUser_whenAddressTypeNotFound() {
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    final var response = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    response.statusCode(is(201));
    StagingUser stagingUser = response.extract().response().as(StagingUser.class);

    var resp = stagingUsersClient.attemptToMergeStagingUser(stagingUser.getId(), null);
    resp.statusCode(is(500));
    assertThat(resp.extract().asString(), is("unable to find address with home as address type"));
  }

  @Test
  void testMergeUser_whenRemotePatronGroupNotFound() {
    createAddressType(HOME);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    final var response = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    response.statusCode(is(201));
    StagingUser stagingUser = response.extract().response().as(StagingUser.class);

    var resp = stagingUsersClient.attemptToMergeStagingUser(stagingUser.getId(), null);
    resp.statusCode(is(500));
    assertThat(resp.extract().asString(), is("unable to find patron group with Remote Non-circulating as group"));
  }

  @Test
  void testMergeUser_withInvalidStagingUserId() {
    createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    var randomId = UUID.randomUUID().toString();
    var resp = stagingUsersClient.attemptToMergeStagingUser(randomId, null);
    resp.statusCode(is(404));
    assertThat(resp.extract().asString(), is(String.format(STAGING_USER_NOT_FOUND, randomId)));
  }

  @Test
  void testMergeUser_withInvalidUserId() {
    createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    var response = stagingUsersClient.attemptToCreateStagingUser(getDummyStagingUser(createRandomString()));
    response.statusCode(is(201));
    StagingUser stagingUser = response.extract().response().as(StagingUser.class);
    var randomId = UUID.randomUUID().toString();
    var resp = stagingUsersClient.attemptToMergeStagingUser(stagingUser.getId(), randomId);
    resp.statusCode(is(404));
    assertThat(resp.extract().asString(), is(String.format(USER_NOT_FOUND, randomId)));
  }

  @Test
  void testMergeUser_withValidStagingUserId() {
    var homeAddressTypeId = createAddressType(HOME);
    var remotePatronGroupId = createPatronGroup(REMOTE_NON_CIRCULATING);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    StagingUser stagingUser = createStagingUser(stagingUserToCreate);
    var updatedDate = stagingUser.getMetadata().getUpdatedDate();

    var newUser = mergeStagingUserAndFetch(stagingUser.getId(), null);

    verifyUserDetails(stagingUserToCreate, newUser, homeAddressTypeId, updatedDate, remotePatronGroupId);

    assertEquals(
      LocalDate.now(ZoneId.systemDefault()).plusYears(2),
      newUser.getExpirationDate().withZoneSameInstant(ZoneId.systemDefault()).toLocalDate(),
      "Expiration date should be 2 years from today (ignoring time)");

    var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("id="+stagingUser.getId());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);
    assertEquals(0, stagingUserdataCollection.getStagingUsers().size(), "staging users should not be present after merge");
  }

  @Test
  void testMergeUser_withValidStagingIdAndUserId_WithoutHomeAddress() {
    var homeAddressTypeId = createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    StagingUser stagingUser = createStagingUser(stagingUserToCreate);
    var updatedDate = stagingUser.getMetadata().getUpdatedDate();
    var workAddressTypeId = createAddressType("Work");
    var patronGroupId = createPatronGroup("patron");
    var existingUser = createUser(patronGroupId, List.of(createAddress(workAddressTypeId)));
    assertEquals(1, existingUser.getPersonal().getAddresses().size());

    var mergedUser = mergeStagingUserAndFetch(stagingUser.getId(), existingUser.getId());

    assertEquals(existingUser.getId(), mergedUser.getId(), "Returned userId should match the query param");
    assertEquals(2, mergedUser.getPersonal().getAddresses().size(), "New address should be added in the address list");
    verifyUserDetails(stagingUserToCreate, mergedUser, homeAddressTypeId, updatedDate, patronGroupId);
    assertNull(mergedUser.getExpirationDate(), "Expiration date should be null for an existing user as it is not set");

    var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("id="+stagingUser.getId());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);
    assertEquals(0, stagingUserdataCollection.getStagingUsers().size(), "staging users should not be present after merge");
  }

  @Test
  void testMergeUser_withValidStagingIdAndUserId_WithHomeAddress() {
    var homeAddressTypeId = createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    StagingUser stagingUser = createStagingUser(stagingUserToCreate);
    var updatedDate = stagingUser.getMetadata().getUpdatedDate();
    var patronGroupId = createPatronGroup("patron");
    var existingUser = createUser(patronGroupId, List.of(createAddress(homeAddressTypeId)));

    var mergedUser = mergeStagingUserAndFetch(stagingUser.getId(), existingUser.getId());

    assertEquals(existingUser.getId(), mergedUser.getId(), "Returned userId should match the query param");
    assertEquals(1, existingUser.getPersonal().getAddresses().size(), "Existing home address should be modified");
    verifyUserDetails(stagingUserToCreate, mergedUser, homeAddressTypeId, updatedDate, patronGroupId);
    assertNull(mergedUser.getExpirationDate(), "Expiration date should be null for an existing user as it is not set");

    var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("id="+stagingUser.getId());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);
    assertEquals(0, stagingUserdataCollection.getStagingUsers().size(), "staging users should not be present after merge");
  }

  @Test
  void testMergeUser_withValidStagingIdAndUserId_WithHomeAndWorkAddress() {
    var homeAddressTypeId = createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    StagingUser stagingUser = createStagingUser(stagingUserToCreate);
    var updatedDate = stagingUser.getMetadata().getUpdatedDate();
    var patronGroupId = createPatronGroup("patron");
    var workAddressTypeId = createAddressType("Work");
    var existingUser = createUser(patronGroupId, List.of(createAddress(homeAddressTypeId), createAddress(workAddressTypeId)));
    assertEquals(2, existingUser.getPersonal().getAddresses().size());

    var mergedUser = mergeStagingUserAndFetch(stagingUser.getId(), existingUser.getId());

    assertEquals(existingUser.getId(), mergedUser.getId(), "Returned userId should match the query param");
    assertEquals(2, mergedUser.getPersonal().getAddresses().size(), "Existing home address should be modified");
    verifyUserDetails(stagingUserToCreate, mergedUser, homeAddressTypeId, updatedDate, patronGroupId);
    assertNull(mergedUser.getExpirationDate(), "Expiration date should be null for an existing user as it is not set");

    var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("id="+stagingUser.getId());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);
    assertEquals(0, stagingUserdataCollection.getStagingUsers().size(), "staging users should not be present after merge");
  }

  @Test
  void testMergeUser_withValidStagingIdAndUserId_WithOutAddress() {
    var homeAddressTypeId = createAddressType(HOME);
    createPatronGroup(REMOTE_NON_CIRCULATING);
    StagingUser stagingUserToCreate = getDummyStagingUser(createRandomString());
    StagingUser stagingUser = createStagingUser(stagingUserToCreate);
    var updatedDate = stagingUser.getMetadata().getUpdatedDate();
    var patronGroupId = createPatronGroup("patron");
    var existingUser = createUser(patronGroupId, List.of());
    assertEquals(0, existingUser.getPersonal().getAddresses().size());

    var mergedUser = mergeStagingUserAndFetch(stagingUser.getId(), existingUser.getId());

    assertEquals(existingUser.getId(), mergedUser.getId(), "Returned userId should match the query param");
    assertEquals(1, mergedUser.getPersonal().getAddresses().size(), "New home address should be added");
    verifyUserDetails(stagingUserToCreate, mergedUser, homeAddressTypeId, updatedDate, patronGroupId);
    assertNull(mergedUser.getExpirationDate(), "Expiration date should be null for an existing user as it is not set");

    var stagingUsersResponse =
      stagingUsersClient.attemptToGetUsers("id="+stagingUser.getId());
    stagingUsersResponse.statusCode(is(200));
    StagingUserdataCollection stagingUserdataCollection = stagingUsersResponse.extract().response().as(StagingUserdataCollection.class);
    assertEquals(0, stagingUserdataCollection.getStagingUsers().size(), "staging users should not be present after merge");
  }

  private String createAddressType(String addressType) {
    return addressTypesClient.attemptToCreateAddressType(
        AddressType.builder()
          .addressType(addressType)
          .build())
      .statusCode(is(HTTP_CREATED))
      .extract().response().as(AddressType.class).getId();
  }

  private String createPatronGroup(String group) {
    return groupsClient.attemptToCreateGroup(Group.builder()
        .group(group)
        .desc(group)
        .expirationOffsetInDays(730)
        .build())
      .statusCode(is(HTTP_CREATED))
      .extract().response().as(Group.class).getId();
  }

  private User createUser(String patronGroupId, List<Address> addressList) {
    final var userToCreate = User.builder()
      .active(true)
      .id("999fd1a4-1865-4991-ae9d-6c9f75d4b043")
      .patronGroup(patronGroupId)
      .personal(Personal.builder()
        .firstName("julia")
        .preferredFirstName("jules")
        .middleName("jj")
        .lastName("brockhurst")
        .email("folio@folio.com")
        .preferredContactTypeId("003")
        .phone("1234")
        .mobilePhone("4321")
        .addresses(addressList)
        .build())
      .preferredEmailCommunication(Set.of(PreferredEmailCommunication.SUPPORT))
      .build();

    final var createdUser = usersClient.createUser(userToCreate);

    final var personal = createdUser.getPersonal();

    assertEquals("brockhurst", personal.getLastName());
    assertEquals("julia", personal.getFirstName());
    assertEquals("jules", personal.getPreferredFirstName());
    assertEquals("jj", personal.getMiddleName());
    assertEquals("folio@folio.com", personal.getEmail());
    assertEquals("1234", personal.getPhone());
    assertEquals("4321", personal.getMobilePhone());
    assertEquals("003", personal.getPreferredContactTypeId());
    assertEquals(addressList.size(), personal.getAddresses().size());
    assertEquals(Set.of(PreferredEmailCommunication.SUPPORT), createdUser.getPreferredEmailCommunication());
    return userToCreate;
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

  private void deleteAllStagingUsers(Vertx vertx) {
    DBTestUtil.deleteFromTable(vertx, "staging_users", TENANT_NAME);
  }

  private StagingUser createStagingUser(StagingUser stagingUserToCreate) {
    final var response = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    response.statusCode(is(201));
    return response.extract().response().as(StagingUser.class);
  }

  private User mergeStagingUserAndFetch(String stagingUserId, String userId) {
    var response = stagingUsersClient.attemptToMergeStagingUser(stagingUserId, userId);
    response.statusCode(is(200));
    return usersClient.getUser(response.extract().as(StagingUserMergeResponse.class).getUserId());
  }

  private void verifyUserDetails(StagingUser stagingUser, User user, String homeAddressTypeId, Date updatedDate, String patronGroupId) {
    var stagingAddressInfo = stagingUser.getAddressInfo();
    var stagingGeneralInfo = stagingUser.getGeneralInfo();
    var stagingContactInfo = stagingUser.getContactInfo();
    var userPersonal = user.getPersonal();
    assertEquals(stagingUser.getPreferredEmailCommunication(), user.getPreferredEmailCommunication());
    assertEquals(stagingGeneralInfo.getFirstName(), userPersonal.getFirstName());
    assertEquals(stagingGeneralInfo.getLastName(), userPersonal.getLastName());
    assertEquals(stagingGeneralInfo.getMiddleName(), userPersonal.getMiddleName());
    assertEquals(stagingGeneralInfo.getPreferredFirstName(), userPersonal.getPreferredFirstName());
    assertEquals(stagingContactInfo.getEmail(), userPersonal.getEmail());
    assertEquals(stagingContactInfo.getEmail(), user.getExternalSystemId());
    assertEquals(stagingContactInfo.getPhone(), userPersonal.getPhone());
    assertEquals(stagingContactInfo.getMobilePhone(), userPersonal.getMobilePhone());
    assertEquals(CONTACT_TYPE_EMAIL_ID, userPersonal.getPreferredContactTypeId(), "user should always have 002 as its contact type");
    var address = fetchPrimaryAddressFromUser(user);
    assertEquals(stagingAddressInfo.getAddressLine0(), address.getAddressLine1());
    assertEquals(stagingAddressInfo.getAddressLine1(), address.getAddressLine2());
    assertEquals(stagingAddressInfo.getCity(), address.getCity());
    assertEquals(stagingAddressInfo.getZip(), address.getPostalCode());
    assertEquals(stagingAddressInfo.getProvince(), address.getRegion());
    assertEquals(stagingAddressInfo.getCountry(), address.getCountryId());
    assertEquals(homeAddressTypeId, address.getAddressTypeId(), "user should always have home address type");
    assertEquals(updatedDate, user.getEnrollmentDate(), "user enrollment date should match the updated date of the staging user");
    assertEquals(patronGroupId, user.getPatronGroup(), "User should have the expected patron group");
    assertTrue(user.getActive());
  }

  private Address fetchPrimaryAddressFromUser(User user) {
    return user.getPersonal().getAddresses()
      .stream()
      .filter(Address::getPrimaryAddress)
      .findFirst()
      .get();
  }

  private Address createAddress(String addressTypeId) {
    return Address.builder()
      .primaryAddress(true)
      .addressLine1("line1")
      .addressLine2("line2")
      .city("kmu")
      .region("TN")
      .postalCode("612001")
      .primaryAddress(true)
      .countryId("IN")
      .addressTypeId(addressTypeId)
      .build();
  }

  private String createRandomString() {
    return RandomStringUtils.random(5, true, true);
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

    var updatedNewStagingUserResponse = stagingUsersClient.attemptToUpdateStagingUser(createdUser.getExternalSystemId(), createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    StagingUser updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertTrue(updatedUser.getPreferredEmailCommunication().containsAll(stagingUserToCreate.getPreferredEmailCommunication()));

    createdUser.setPreferredEmailCommunication(Set.of(PreferredEmailCommunication.PROGRAMS));
    updatedNewStagingUserResponse = stagingUsersClient.attemptToUpdateStagingUser(createdUser.getExternalSystemId(), createdUser);
    updatedNewStagingUserResponse.statusCode(is(200));
    updatedUser = updatedNewStagingUserResponse.extract().response().as(StagingUser.class);

    assertEquals(1, updatedUser.getPreferredEmailCommunication().size());
    assertTrue(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.PROGRAMS));
    assertFalse(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.SERVICES));
    assertFalse(updatedUser.getPreferredEmailCommunication().contains(PreferredEmailCommunication.SUPPORT));
  }
}
