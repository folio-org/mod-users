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

  public StagingUserService(Context vertxContext, Map<String, String> okapiHeaders) {
    this.vertxContext = vertxContext;
    this.okapiHeaders = okapiHeaders;
  }

  public Future<User> mergeStagingUserWithUser(String stagingUserId, String userId) {
    log.debug("mergeStagingUserWithUser:: Merging staging user Details with id {} to user with id {}",
      stagingUserId, userId);
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .withTrans(conn -> Future.all(fetchHomeAddressType(conn), fetchRemotePatronGroupOnlyIfUserIdPresent(conn, userId))
        .compose(compositeFuture -> {
          AddressType homeAddress = compositeFuture.resultAt(0);
          String homeAddressId = homeAddress.getId();

          Usergroup remotePatronGroup = compositeFuture.resultAt(1);
          String patronGroupId = remotePatronGroup != null ? remotePatronGroup.getId() : null;

          return fetchStagingUserById(conn, stagingUserId)
            .compose(stagingUser -> handleStagingUser(stagingUser, userId, conn, homeAddressId, patronGroupId));
        }))
      .onFailure(t -> log.error("mergeStagingUserWithUser:: Merge failed for stagingUserId {}, userId {}: {}",
        stagingUserId, userId, t.getMessage()));
  }

  private Future<AddressType> fetchHomeAddressType(Conn conn) {
    log.debug("fetchHomeAddressType:: fetching address of type home");
    return fetchEntityByFieldNameAndValue(conn, ADDRESS_TYPE, HOME, AddressType.class, ADDRESS_TYPE_TABLE,
      HOME_ADDRESS_TYPE_NOT_FOUND);
  }

  private Future<Usergroup> fetchRemotePatronGroupOnlyIfUserIdPresent(Conn conn, String userId) {
    log.debug("fetchRemotePatronGroupOnlyIfUserIdPresent:: fetching Remote patron group with userId {}", userId);
    return userId != null
      ? fetchEntityByFieldNameAndValue(conn, GROUP, REMOTE_NON_CIRCULATING,
      Usergroup.class, GROUP_TABLE, REMOTE_PATRON_GROUP_NOT_FOUND)
      : Future.succeededFuture(null);
  }

  private Future<StagingUser> fetchStagingUserById(Conn conn, String stagingUserId) {
    log.debug("fetchStagingUserById:: fetching staging user with id {}", stagingUserId);
    return fetchEntityByFieldNameAndValue(conn, ID, stagingUserId, StagingUser.class, STAGING_USERS_TABLE, STAGING_USER_NOT_FOUND);
  }

  private <T> Future<T> fetchEntityByFieldNameAndValue(Conn conn, String fieldName, String fieldValue, Class<T> clazz, String tableName, String errorMessage) {
    log.debug("fetchEntityByFieldNameAndValue:: Fetching entity {} with field {} and value {}",
      clazz.getSimpleName(), fieldName, fieldValue);
    var criterion = new Criterion(new Criteria().addField("'" + fieldName + "'")
      .setOperation("=").setVal(fieldValue));
    return conn.get(tableName, clazz, criterion)
      .compose(results -> {
        if (!results.getResults().isEmpty()) {
          return succeededFuture(results.getResults().get(0));
        } else {
          log.warn("fetchEntityByFieldNameAndValue:: Entity not found: {} with field {} and value: {}",
            clazz.getSimpleName(), fieldName, fieldValue);
          return Future.failedFuture(String.format(errorMessage, fieldValue));
        }
      })
      .onFailure(t -> log.error("fetchEntityByFieldNameAndValue:: Error fetching {} from table {}: {}",
        clazz.getSimpleName(), tableName, t.getMessage()));
  }

  public Future<User> handleStagingUser(StagingUser stagUser, String userId, Conn conn, String homeAddressTypeId,
                                        String patronGroupId) {
    log.debug("handleStagingUser:: handle staging user {} with userId {} , homeAddressId {} , patronGroupId {}",
      stagUser, userId, homeAddressTypeId, patronGroupId);
    var userService = new UsersService();
    if (userId == null) {
      log.info("handleStagingUser:: User id is null, hence creating a new user from staging user with id {}",
        stagUser.getId());
      var newUser = createUserFromStagingUser(stagUser, homeAddressTypeId, patronGroupId);
      return userService.saveAndReturnUser(conn, newUser);
    } else {
      log.info("handleStagingUser:: Fetching userId {} and updating the user from staging user with id {}",
        userId, stagUser.getId());
      return userService.getUserById(conn, userId)
        .compose(existingUser -> {
          if (existingUser != null) {
            var updatedUser = updateUserFromStagingUser(stagUser, existingUser, homeAddressTypeId);
            return userService.updateUser(conn, updatedUser);
          } else {
            return Future.failedFuture(String.format(USER_NOT_FOUND, userId));
          }
        });
    }
  }

  private User createUserFromStagingUser(StagingUser stagingUser, String homeAddressTypeId, String patronGroupId) {
    User user = new User();
    user.setId(UUID.randomUUID().toString());
    user.setActive(true);
    user.setPatronGroup(patronGroupId);
    user.setPreferredEmailCommunication(stagingUser.getPreferredEmailCommunication());
    LocalDate currentDate = LocalDate.now();
    LocalDate expirationLocalDate = currentDate.plusYears(2);
    Date expirationDate = Date.from(expirationLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
    user.setExpirationDate(expirationDate);
    user.setType(PATRON_TYPE);
    user.setExternalSystemId(stagingUser.getContactInfo() != null ? stagingUser.getContactInfo().getEmail() : null);
    user.setEnrollmentDate(stagingUser.getMetadata().getUpdatedDate());
    Personal personal = createOrUpdatePersonal(stagingUser, new Personal(), homeAddressTypeId);
    user.setPersonal(personal);
    user.setMetadata(MetadataUtil.createMetadata(okapiHeaders));
    return user;
  }

  private User updateUserFromStagingUser(StagingUser stagingUser, User user, String homeAddressTypeId) {
    user.setActive(true);
    user.setExternalSystemId(stagingUser.getContactInfo() != null ? stagingUser.getContactInfo().getEmail() : null);
    user.setEnrollmentDate(stagingUser.getMetadata().getUpdatedDate());
    Personal personal = createOrUpdatePersonal(stagingUser, user.getPersonal(), homeAddressTypeId);
    user.setPersonal(personal);
    return user;
  }

  private Personal createOrUpdatePersonal(StagingUser stagingUser, Personal personal, String homeAddressTypeId) {
    var contactInfo = stagingUser.getContactInfo();
    var generalInfo = stagingUser.getGeneralInfo();
    var addressInfo = stagingUser.getAddressInfo();

    setContactInfo(contactInfo, personal);
    setGeneralInfo(generalInfo, personal);
    Address homeAddress = personal.getAddresses() != null
      ? personal.getAddresses().stream()
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
      personal.setEmail(contactInfo.getEmail());
      personal.setPhone(contactInfo.getPhone());
      personal.setMobilePhone(contactInfo.getMobilePhone());
      personal.setPreferredContactTypeId(CONTACT_TYPE_EMAIL_ID);
    }
  }

  private void setGeneralInfo(GeneralInfo generalInfo, Personal personal) {
    if (generalInfo != null) {
      personal.setFirstName(generalInfo.getFirstName());
      personal.setLastName(generalInfo.getLastName());
      personal.setMiddleName(generalInfo.getMiddleName());
      personal.setPreferredFirstName(generalInfo.getPreferredFirstName());
    }
  }

  private void updateAddress(Address userAddress, AddressInfo addressInfo, String homeAddressTypeId) {
    if (userAddress.getId() == null) {
      userAddress.setId(UUID.randomUUID().toString());
    }
    userAddress.setCountryId(addressInfo.getCountry());
    userAddress.setAddressLine1(addressInfo.getAddressLine0());
    userAddress.setAddressLine2(addressInfo.getAddressLine1());
    userAddress.setCity(addressInfo.getCity());
    userAddress.setRegion(addressInfo.getProvince());
    userAddress.setPostalCode(addressInfo.getZip());
    userAddress.setAddressTypeId(homeAddressTypeId);
    userAddress.setPrimaryAddress(true);
  }
}
