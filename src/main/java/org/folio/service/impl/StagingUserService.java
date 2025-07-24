package org.folio.service.impl;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.Json;
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
import org.folio.support.StagingUserUpdatesStorage;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.impl.AddressTypeAPI.ADDRESS_TYPE_TABLE;
import static org.folio.rest.impl.StagingUsersAPI.STAGING_USERS_TABLE;
import static org.folio.rest.impl.UserGroupAPI.GROUP_TABLE;
import static org.folio.service.event.EntityChangedEventPublisherFactory.userEventPublisher;
import static org.folio.support.UsersApiConstants.ID;

public class StagingUserService {

  public static final String PATRON_TYPE = "patron";
  public static final String CONTACT_TYPE_EMAIL_ID = "002";
  public static final String USER_NOT_FOUND = "user with id %s not found";
  public static final String STAGING_USER_NOT_FOUND = "staging user with id %s not found";
  public static final String HOME_ADDRESS_TYPE_NOT_FOUND = "unable to find address with home as address type";
  public static final String PATRON_GROUP_NOT_FOUND = "unable to find patron group with %s as group";
  public static final String ADDRESS_TYPE = "addressType";
  public static final String HOME = "Home";
  public static final String GROUP = "group";
  public static final String REMOTE_NON_CIRCULATING = "Remote Non-circulating";
  public static final String BASIC_MINOR_INTERNAL_PATRON_GROUP = "Basic -- Minor (INTERNAL)";
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
      .withTrans(conn -> fetchStagingUserById(conn, stagingUserId)
        .compose(stagingUser -> {

          List<Future<?>> requiredFetches = List.of(
            fetchHomeAddressType(conn),
            fetchRemotePatronGroupOnlyIfNotMinorAndUserIdNotPresent(conn, userId, stagingUser.getMinor()),
            fetchBasicMinorPatronGroupOnlyIfMinorTrue(conn, stagingUser.getMinor())
          );

          return Future.all(requiredFetches).compose(compositeFuture -> {
            AddressType homeAddress = compositeFuture.resultAt(0);
            String homeAddressId = homeAddress.getId();
            Usergroup remotePatronGroup = compositeFuture.resultAt(1);
            Usergroup basicMinorPatronGroup = compositeFuture.resultAt(2);

            return getCreateOrUpdateUser(userId, conn, stagingUser, homeAddressId, basicMinorPatronGroup,
              remotePatronGroup).compose(userUpdatesStorage -> deleteStagingUser(conn, stagingUserId,
              userUpdatesStorage));
          });
        })).onSuccess(userUpdatesStorage -> publishUserEvent(userId, userUpdatesStorage))
      .compose(userUpdatesStorage -> succeededFuture(userUpdatesStorage.getNewEntity()))
      .onFailure(t -> log.error("mergeOrCreateUserFromStagingUser:: Merge or creation failed for stagingUserId {}, userId {}: {}",
        stagingUserId, userId, t.getMessage()));
  }

  private Future<StagingUserUpdatesStorage<User>> getCreateOrUpdateUser(String userId, Conn conn,
    StagingUser stagingUser, String homeAddressId, Usergroup basicMinorPatronGroup, Usergroup remotePatronGroup) {
    return (userId != null ?
      updateExistingUserDetailsFromStagingUser(stagingUser, userId, conn, homeAddressId, basicMinorPatronGroup)
      : createNewUserFromStagingUser(stagingUser, conn, homeAddressId, remotePatronGroup, basicMinorPatronGroup));
  }

  private void publishUserEvent(String userId, StagingUserUpdatesStorage<User> userUpdatesStorage) {
    if (userId == null) {
      userEventPublisher(vertxContext, okapiHeaders)
        .publishCreated(userUpdatesStorage.getNewEntity().getId(), userUpdatesStorage.getNewEntity());
    } else {
      userEventPublisher(vertxContext, okapiHeaders)
        .publishUpdated(userId, userUpdatesStorage.getOldEntity(), userUpdatesStorage.getNewEntity());
    }
  }

  private Future<AddressType> fetchHomeAddressType(Conn conn) {
    log.debug("fetchHomeAddressType:: fetching address of type home");
    return fetchEntityByCriterion(conn, ADDRESS_TYPE, HOME, AddressType.class, ADDRESS_TYPE_TABLE,
      HOME_ADDRESS_TYPE_NOT_FOUND);
  }

  private Future<Usergroup> fetchRemotePatronGroupOnlyIfNotMinorAndUserIdNotPresent(Conn conn, String userId, Boolean minor) {
    log.debug("fetchRemotePatronGroupOnlyIfUserIdNotPresent:: fetching Remote patron group with userId {}", userId);
    return isUserIdNullAndNotMinor(userId, minor)
      ? fetchEntityByCriterion(conn, GROUP, REMOTE_NON_CIRCULATING,
      Usergroup.class, GROUP_TABLE, String.format(PATRON_GROUP_NOT_FOUND, REMOTE_NON_CIRCULATING))
      : Future.succeededFuture(null);
  }

  private static boolean isUserIdNullAndNotMinor(String userId, Boolean minor) {
    return userId == null && (Objects.isNull(minor) || !minor);
  }

  private Future<Usergroup> fetchBasicMinorPatronGroupOnlyIfMinorTrue(Conn conn, Boolean minor) {
    log.debug("fetchBasicMinorPatronGroup:: fetching {} patron group", BASIC_MINOR_INTERNAL_PATRON_GROUP);
    return Boolean.TRUE.equals(minor) ?
      fetchEntityByCriterion(conn, GROUP, BASIC_MINOR_INTERNAL_PATRON_GROUP,
        Usergroup.class, GROUP_TABLE, String.format(PATRON_GROUP_NOT_FOUND, BASIC_MINOR_INTERNAL_PATRON_GROUP))
      : Future.succeededFuture(null);
  }

  private Future<Usergroup> fetchRemotePatronGroupById(Conn conn, String patronGroupId) {
    log.debug("fetchRemotePatronGroupById:: fetching patronGroup with id {}", patronGroupId);
    String patronGroupErrorMsg = String.format("Unable to find the patron group by id: %s", patronGroupId);
    return fetchEntityByCriterion(conn, ID, patronGroupId, Usergroup.class, GROUP_TABLE, patronGroupErrorMsg);
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

  private Future<StagingUserUpdatesStorage<User>> createNewUserFromStagingUser(StagingUser stagUser, Conn conn,
                                                                               String homeAddressTypeId,
                                                                               Usergroup remotePatronGroup, Usergroup basicMinorPatronGroup) {
    log.debug("createNewUserFromStagingUser:: creating a new user from staging user {}", stagUser);
    var newUser = createUserEntityFromStagingUser(stagUser, homeAddressTypeId);
    setUserPatronGroupAndExpirationDate(newUser, stagUser, remotePatronGroup, basicMinorPatronGroup, true);
    return usersService.saveAndReturnUser(conn, newUser)
            .compose(user->Future.succeededFuture(new StagingUserUpdatesStorage<>(user)));
  }

  private Future<StagingUserUpdatesStorage<User>> updateExistingUserDetailsFromStagingUser(StagingUser stagUser, String userId,
                                                                                           Conn conn, String homeAddressTypeId,
                                                                                           Usergroup basicMinorPatronGroup) {
    log.debug("updateExistingUserDetailsFromStagingUser:: updating existing user {} with stagUser details {}",
      userId, stagUser);
    return usersService.getUserById(conn, userId)
      .compose(existingUser -> {
        if (existingUser != null) {
          User oldEntity = Json.decodeValue(Json.encode(existingUser), User.class);
          var updatedUser = updateUserFromStagingUser(stagUser, existingUser, homeAddressTypeId);
          setUserPatronGroupAndExpirationDate(updatedUser, stagUser, null, basicMinorPatronGroup, false);
          return fetchRemotePatronGroupById(conn, existingUser.getPatronGroup())
                  .compose(usergroup -> {
                    setUserExpirationDate(updatedUser, updatedUser.getEnrollmentDate(), usergroup);
                    return usersService.updateUser(conn, updatedUser)
                            .compose(user -> Future.succeededFuture(new StagingUserUpdatesStorage<User>(true,
                                    oldEntity, user)));
                  });
        } else {
          return Future.failedFuture(String.format(USER_NOT_FOUND, userId));
        }
      });
  }

  private User createUserEntityFromStagingUser(StagingUser stagingUser, String homeAddressTypeId) {
    return new User()
      .withId(UUID.randomUUID().toString())
      .withActive(true)
      .withPreferredEmailCommunication(stagingUser.getPreferredEmailCommunication())
      .withEnrollmentDate(stagingUser.getMetadata().getUpdatedDate())
      .withType(PATRON_TYPE)
      .withExternalSystemId(stagingUser.getExternalSystemId())
      .withPersonal(createOrUpdatePersonal(stagingUser, new Personal(), homeAddressTypeId))
      .withMetadata(MetadataUtil.createMetadata(okapiHeaders));

  }

  private void setUserPatronGroupAndExpirationDate(User user, StagingUser stagingUser,
                                                   Usergroup remotePatronGroup, Usergroup basicMinorPatronGroup, boolean isNewUser) {
    if (Boolean.TRUE.equals(stagingUser.getMinor())) {
      user.setPatronGroup(basicMinorPatronGroup.getId());
      setUserExpirationDate(user, user.getEnrollmentDate(), basicMinorPatronGroup);
    } else if (isNewUser &&  Objects.nonNull(remotePatronGroup)) {
      user.setPatronGroup(remotePatronGroup.getId());
      setUserExpirationDate(user, user.getEnrollmentDate(), remotePatronGroup);
    }
  }

  private void setUserExpirationDate(User newUser, Date enrollmentDate, Usergroup userPatronGroup) {
    if(isPatronGroupHasExpirationDateOffset(userPatronGroup)) {
      ZonedDateTime zonedDateTime = enrollmentDate.toInstant()
              .atZone(ZoneId.systemDefault())
              .plusDays(userPatronGroup.getExpirationOffsetInDays());
      newUser.setExpirationDate(Date.from(zonedDateTime.toInstant()));
    }
  }

  private boolean isPatronGroupHasExpirationDateOffset(Usergroup remotePatronGroup) {
    return Objects.nonNull(remotePatronGroup) && remotePatronGroup.getExpirationOffsetInDays() != null;
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

  private Future<StagingUserUpdatesStorage<User>> deleteStagingUser(Conn conn, String stagingUserId,
                                                                    StagingUserUpdatesStorage<User> stagingUserUpdatesStorage) {
    return conn.delete(STAGING_USERS_TABLE, stagingUserId)
      .compose(rowSet -> rowSet.size() !=0 ? failedFuture(String.format("Unable to delete the staging user %s", stagingUserId))
      : Future.succeededFuture(stagingUserUpdatesStorage));
  }

}
