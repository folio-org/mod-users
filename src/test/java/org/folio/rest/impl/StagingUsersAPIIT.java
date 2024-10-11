
package org.folio.rest.impl;

import com.fasterxml.jackson.databind.JsonMappingException;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import lombok.SneakyThrows;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.domain.UserType;
import org.folio.moduserstest.AbstractRestTestNoData;
import org.folio.rest.jaxrs.model.AddressInfo;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.GeneralInfo;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.persist.PostgresClient;
import org.folio.support.Address;
import org.folio.support.AddressType;
import org.folio.support.Personal;
import org.folio.support.TagList;
import org.folio.support.User;
import org.folio.support.ValidationErrors;
import org.folio.support.http.AddressTypesClient;
import org.folio.support.http.ConfigurationClient;
import org.folio.support.http.GroupsClient;
import org.folio.support.http.StagingUsersClient;
import org.folio.support.http.TimerInterfaceClient;
import org.folio.support.http.UserProfilePictureClient;
import org.folio.support.http.UserTenantClient;
import org.folio.support.http.UsersClient;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.testcontainers.shaded.org.awaitility.Awaitility;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.PROGRAMS;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.SERVICES;
import static org.folio.rest.jaxrs.model.PreferredEmailCommunication.SUPPORT;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertThrows;

@Timeout(value = 20, timeUnit = SECONDS)
@ExtendWith(VertxExtension.class)
class StagingUsersAPIIT extends AbstractRestTestNoData {

  private static StagingUsersClient stagingUsersClient;


  @BeforeAll
  @SneakyThrows
  static void beforeAll() {
    stagingUsersClient = new StagingUsersClient(okapiUrl, okapiHeaders);
  }

  @BeforeEach
  public void beforeEach() {
//    stagingUsersClient.deleteAllUsers();
  }

  @Test
  void canCreateStagingUser() {
    StagingUser stagingUserToCreate = getDummyStagingUser();
    final var response = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    response.statusCode(is(201));
    StagingUser createdUser = response.extract().response().as(StagingUser.class);

    assertThat(createdUser.getId(), is(notNullValue()));
    assertThat(createdUser.getContactInfo().getEmail(), is(stagingUserToCreate.getContactInfo().getEmail()));
    assertThat(createdUser.getMetadata().getCreatedDate(), is(notNullValue()));
    assertThat(createdUser.getMetadata().getUpdatedDate(), is(notNullValue()));

    stagingUserToCreate.getGeneralInfo().setFirstName("welcome");
    final var response1 = stagingUsersClient.attemptToCreateStagingUser(stagingUserToCreate);
    response1.statusCode(is(200));
    StagingUser createdUser1 = response.extract().response().as(StagingUser.class);
    assertThat(createdUser1.getGeneralInfo().getFirstName(), is(stagingUserToCreate.getGeneralInfo().getFirstName()));
  }

  @NotNull
  private static StagingUser getDummyStagingUser() {
    StagingUser stagingUserToCreate =  new StagingUser();
    GeneralInfo generalInfo = new GeneralInfo();
    generalInfo.setFirstName("DKapil");
    generalInfo.setLastName("DSony");
    generalInfo.setPreferredFirstName("DPreferredFirstName");
    generalInfo.setMiddleName("DMiddle Name");
    stagingUserToCreate.setGeneralInfo(generalInfo);

    ContactInfo contactInfo = new ContactInfo();
    contactInfo.setMobilePhone("Dmobile");
    contactInfo.setPhone("Dphone");
    contactInfo.setEmail("email@email.com");
    stagingUserToCreate.setContactInfo(contactInfo);

    AddressInfo addressInfo = new AddressInfo();
    addressInfo.setAddressLine0("DsetAddressLine0");
    addressInfo.setAddressLine1("DsetAddressLine1");
    addressInfo.setCity("DCity");
    addressInfo.setProvince("Dprovince");
    addressInfo.setCountry("Dcountry");
    addressInfo.setZip("D123456");
    stagingUserToCreate.setAddressInfo(addressInfo);
    return stagingUserToCreate;
  }


}
