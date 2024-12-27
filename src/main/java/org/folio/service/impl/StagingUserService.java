package org.folio.service.impl;

import io.vertx.core.Context;
import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressInfo;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.GeneralInfo;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.tools.utils.MetadataUtil;
import org.folio.service.UsersService;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.impl.AddressTypeAPI.ADDRESS_TYPE_TABLE;
import static org.folio.rest.impl.StagingUsersAPI.STAGING_USERS_TABLE;
import static org.folio.rest.impl.UserGroupAPI.GROUP_TABLE;
import static org.folio.support.UsersApiConstants.ID;

public class StagingUserService {

  public static final String PATRON_TYPE = "patron";
  public static final String CONTACT_TYPE_EMAIL_ID = "002";
  public static final String USER_NOT_FOUND = "user with id %s not found";
  public static final String STAGING_USER_NOT_FOUND = "staging user with id %s not found";
  public static final String HOME_ADDRESS_TYPE_NOT_FOUND = "unable to find address with home as address type";
  public static final String REMOTE_PATRON_GROUP_NOT_FOUND = "unable to find patron group with Remote Non-circulating as group";
  public static final String ADDRESS_TYPE = "addressType";
  public static final String HOME = "Home";
  public static final String GROUP = "group";
  public static final String REMOTE_NON_CIRCULATING = "Remote Non-circulating";
  private static final Logger log = LogManager.getLogger(StagingUserService.class);
  private final Context vertxContext;
  private final Map<String, String> okapiHeaders;
  private final UsersService usersService;

  public StagingUserService(Context vertxContext, Map<String, String> okapiHeaders) {
    this.vertxContext = vertxContext;
    this.okapiHeaders = okapiHeaders;
    this.usersService = new UsersService();
  }

  public Future<User> mergeOrCreateUserFromStagingUser(String stagingUserId, String userId) {
    log.debug("mergeOrCreateUserFromStagingUser:: Creating or updating user with id {} from stagingUserId {}",
      userId, stagingUserId);
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .withTrans(conn -> Future.all(fetchHomeAddressType(conn), fetchRemotePatronGroupOnlyIfUserIdNotPresent(conn, userId))
        .compose(compositeFuture -> {
          AddressType homeAddress = compositeFuture.resultAt(0);
          String homeAddressId = homeAddress.getId();

          Usergroup remotePatronGroup = compositeFuture.resultAt(1);
          String patronGroupId = remotePatronGroup != null ? remotePatronGroup.getId() : null;

          return fetchStagingUserById(conn, stagingUserId)
            .compose(stagingUser -> userId != null ? updateExistingUserDetailsFromStagingUser(stagingUser, userId, conn, homeAddressId)
              : createNewUserFromStagingUser(stagingUser, conn, homeAddressId, patronGroupId))
            .compose(user -> deleteStagingUser(conn, stagingUserId, user));
        }))
      .onFailure(t -> log.error("mergeOrCreateUserFromStagingUser:: Merge or creation failed for stagingUserId {}, userId {}: {}",
        stagingUserId, userId, t.getMessage()));
  }

  private Future<AddressType> fetchHomeAddressType(Conn conn) {
    log.debug("fetchHomeAddressType:: fetching address of type home");
    return fetchEntityByCriterion(conn, ADDRESS_TYPE, HOME, AddressType.class, ADDRESS_TYPE_TABLE,
      HOME_ADDRESS_TYPE_NOT_FOUND);
  }

  private Future<Usergroup> fetchRemotePatronGroupOnlyIfUserIdNotPresent(Conn conn, String userId) {
    log.debug("fetchRemotePatronGroupOnlyIfUserIdNotPresent:: fetching Remote patron group with userId {}", userId);
    return userId == null
      ? fetchEntityByCriterion(conn, GROUP, REMOTE_NON_CIRCULATING,
      Usergroup.class, GROUP_TABLE, REMOTE_PATRON_GROUP_NOT_FOUND)
      : Future.succeededFuture(null);
  }

  private Future<StagingUser> fetchStagingUserById(Conn conn, String stagingUserId) {
    log.debug("fetchStagingUserById:: fetching staging user with id {}", stagingUserId);
    return fetchEntityByCriterion(conn, ID, stagingUserId, StagingUser.class, STAGING_USERS_TABLE, STAGING_USER_NOT_FOUND);
  }

  private <T> Future<T> fetchEntityByCriterion(Conn conn, String fieldName, String fieldValue, Class<T> clazz,
                                               String tableName, String errorMessage) {
    log.debug("fetchEntityByCriterion:: Fetching entity {} with field {} and value {}",
      clazz.getSimpleName(), fieldName, fieldValue);
    var criterion = new Criterion(new Criteria().addField("'" + fieldName + "'")
      .setOperation("=").setVal(fieldValue));
    return conn.get(tableName, clazz, criterion)
      .compose(results -> !results.getResults().isEmpty() ?
        succeededFuture(results.getResults().get(0))
        : failedFuture(String.format(errorMessage, fieldValue)))
      .onFailure(t -> log.error("fetchEntityByCriterion:: Error fetching {} from table {}: {}",
        clazz.getSimpleName(), tableName, t.getMessage()));
  }

  private Future<User> createNewUserFromStagingUser(StagingUser stagUser, Conn conn, String homeAddressTypeId,
                                                    String patronGroupId) {
    log.debug("createNewUserFromStagingUser:: creating a new user from staging user {}", stagUser);
    var newUser = createUserEntityFromStagingUser(stagUser, homeAddressTypeId, patronGroupId);
    return usersService.saveAndReturnUser(conn, newUser);
  }

  private Future<User> updateExistingUserDetailsFromStagingUser(StagingUser stagUser, String userId,
                                                                Conn conn, String homeAddressTypeId) {
    log.debug("updateExistingUserDetailsFromStagingUser:: updating existing user {} with stagUser details {}",
      userId, stagUser);
    return usersService.getUserById(conn, userId)
      .compose(existingUser -> {
        if (existingUser != null) {
          var updatedUser = updateUserFromStagingUser(stagUser, existingUser, homeAddressTypeId);
          return usersService.updateUser(conn, updatedUser);
        } else {
          return Future.failedFuture(String.format(USER_NOT_FOUND, userId));
        }
      });
  }

  private User createUserEntityFromStagingUser(StagingUser stagingUser, String homeAddressTypeId, String patronGroupId) {
    return new User()
      .withId(UUID.randomUUID().toString())
      .withActive(true)
      .withPatronGroup(patronGroupId)
      .withPreferredEmailCommunication(stagingUser.getPreferredEmailCommunication())
      .withExpirationDate(Date.from(LocalDate.now().plusYears(2).atStartOfDay(ZoneId.systemDefault()).toInstant()))
      .withEnrollmentDate(stagingUser.getMetadata().getUpdatedDate())
      .withType(PATRON_TYPE)
      .withExternalSystemId(stagingUser.getExternalSystemId())
      .withPersonal(createOrUpdatePersonal(stagingUser, new Personal(), homeAddressTypeId))
      .withMetadata(MetadataUtil.createMetadata(okapiHeaders));

  }

  private User updateUserFromStagingUser(StagingUser stagingUser, User user, String homeAddressTypeId) {
    var metaData = MetadataUtil.createMetadata(okapiHeaders);
    return user.withActive(true)
      .withPreferredEmailCommunication(stagingUser.getPreferredEmailCommunication())
      .withExternalSystemId(stagingUser.getExternalSystemId())
      .withEnrollmentDate(stagingUser.getMetadata().getUpdatedDate())
      .withPersonal(createOrUpdatePersonal(stagingUser, user.getPersonal(), homeAddressTypeId))
      .withMetadata(user.getMetadata().withUpdatedDate(metaData.getUpdatedDate()))
      .withMetadata(user.getMetadata().withUpdatedByUserId(metaData.getUpdatedByUserId()));
  }

  private Personal createOrUpdatePersonal(StagingUser stagingUser, Personal personal, String homeAddressTypeId) {
    var contactInfo = stagingUser.getContactInfo();
    var generalInfo = stagingUser.getGeneralInfo();
    var addressInfo = stagingUser.getAddressInfo();

    setContactInfo(contactInfo, personal);
    setGeneralInfo(generalInfo, personal);
    Address homeAddress = personal.getAddresses() != null
      ? personal.getAddresses().stream()
      .map(address -> address.withPrimaryAddress(false))
      .filter(address -> homeAddressTypeId.equals(address.getAddressTypeId()))
      .findFirst()
      .orElse(new Address())
      : new Address();

    updateAddress(homeAddress, addressInfo, homeAddressTypeId);
    if (personal.getAddresses() == null) {
      personal.setAddresses(new ArrayList<>());
    }
    if (!personal.getAddresses().contains(homeAddress)) {
      personal.getAddresses().add(homeAddress);
    }

    return personal;
  }

  private void setContactInfo(ContactInfo contactInfo, Personal personal) {
    if (contactInfo != null) {
      personal.withEmail(contactInfo.getEmail())
        .withPhone(contactInfo.getPhone())
        .withMobilePhone(contactInfo.getMobilePhone())
        .withPreferredContactTypeId(CONTACT_TYPE_EMAIL_ID);
    }
  }

  private void setGeneralInfo(GeneralInfo generalInfo, Personal personal) {
    if (generalInfo != null) {
      personal.withFirstName(generalInfo.getFirstName())
        .withLastName(generalInfo.getLastName())
        .withMiddleName(generalInfo.getMiddleName())
        .withPreferredFirstName(generalInfo.getPreferredFirstName());
    }
  }

  private void updateAddress(Address userAddress, AddressInfo addressInfo, String homeAddressTypeId) {
    userAddress
      .withId(userAddress.getId() != null ? userAddress.getId() : UUID.randomUUID().toString())
      .withCountryId(addressInfo.getCountry())
      .withAddressLine1(addressInfo.getAddressLine0())
      .withAddressLine2(addressInfo.getAddressLine1())
      .withCity(addressInfo.getCity())
      .withRegion(addressInfo.getProvince())
      .withPostalCode(addressInfo.getZip())
      .withAddressTypeId(homeAddressTypeId)
      .withPrimaryAddress(true);
  }

  private Future<User> deleteStagingUser(Conn conn, String stagingUserId, User user) {
    return conn.delete(STAGING_USERS_TABLE, stagingUserId)
      .compose(rowSet -> rowSet.size() !=0 ? failedFuture(String.format("Unable to delete the staging user %s", stagingUserId))
      : Future.succeededFuture(user));
  }

}
