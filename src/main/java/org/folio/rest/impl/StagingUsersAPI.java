package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressInfo;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.interfaces.Results;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.folio.rest.impl.AddressTypeAPI.ADDRESS_TYPE_TABLE;
import static org.folio.rest.impl.UserGroupAPI.GROUP_TABLE;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {

  public static final String STAGING_USERS_TABLE = "staging_users";
  private static final Logger log = LogManager.getLogger(StagingUsersAPI.class);

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  @Override
  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void putStagingUsersMergeById(String id, String userId, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    Future<String> homeAddressTypeIdFuture = fetchHomeAddressTypeId(vertxContext, okapiHeaders);

    Future<String> patronGroupIdFuture = (userId == null)
      ? fetchPatronGroupId(vertxContext, okapiHeaders)
      : Future.succeededFuture(null);  // Skip fetching if userId is present

    Future.all(homeAddressTypeIdFuture, patronGroupIdFuture)
      .compose(composite -> {
        String homeAddressTypeId = composite.resultAt(0);
        String patronGroupId = composite.resultAt(1);
        log.info("homeAddressTypeId {}", homeAddressTypeId);
        log.info("patronGroupId {}", patronGroupId);
        var criterion1 = new Criterion(new Criteria().addField("id").setJSONB(false).setOperation("=").setVal(id));
        return PgUtil.postgresClient(vertxContext, okapiHeaders)
          .withTrans(conn -> conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion1)
            .compose(stagingUser -> handleStagingUser(stagingUser, userId, conn, id, homeAddressTypeId, patronGroupId)));
      })
      .onSuccess(user -> PutStagingUsersMergeByIdResponse.respond200WithApplicationJson(user.getId()))
      .map(Response.class :: cast)
      .onComplete(asyncResultHandler);
  }

  private Future<String> fetchHomeAddressTypeId(Context vertxContext, Map<String, String> okapiHeaders) {
    var criterion = new Criterion(new Criteria().addField("'addressType'").setOperation("=").setVal("Home"));
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .withTrans(conn -> conn.get(ADDRESS_TYPE_TABLE, AddressType.class, criterion))
      .compose(addressType -> {
        if (!addressType.getResults().isEmpty()) {
          return Future.succeededFuture(addressType.getResults().get(0).getId());
        } else {
          return Future.failedFuture(PutStagingUsersMergeByIdResponse.respond500WithTextPlain("Home address type not found").toString());
        }
      });
  }

  private Future<String> fetchPatronGroupId(Context vertxContext, Map<String, String> okapiHeaders) {
    var criterion = new Criterion(new Criteria().addField("'patronGroup'").setOperation("=").setVal("Remote"));
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .withTrans(conn -> conn.get(GROUP_TABLE, Usergroup.class, criterion))
      .map(patronGroup -> {
        if (!patronGroup.getResults().isEmpty()) {
          return patronGroup.getResults().get(0).getId();
        } else {
          throw new RuntimeException("Patron group 'Remote' not found");
        }
      });
  }

  private Future<User> handleStagingUser(Results<StagingUser> stagingUserResult, String userId, Conn conn, String id, String homeAddressTypeId, String patronGroupId) {
    if (stagingUserResult.getResults().isEmpty()) {
      log.info("Invalid staging user is provided");
      return Future.failedFuture("Staging user not found");
    }

    var stagUser = stagingUserResult.getResults().get(0);

    if (userId == null) {
      var newUser = createUserFromStagingUser(stagUser, homeAddressTypeId, patronGroupId);
      return conn.saveAndReturnUpdatedEntity(TABLE_NAME_USERS, newUser.getId(), newUser)
        .compose(res -> {
          log.info("saving new user {}", res);
          return conn.delete(STAGING_USERS_TABLE, id).map(x -> {
            log.info("Deletion of staging user with id {} happened successfully", id);
            return res;
          });
        });
    } else {
      var criterion2 = new Criterion(new Criteria().addField("id").setJSONB(false).setOperation("=").setVal(userId));
      return conn.get(TABLE_NAME_USERS, User.class, criterion2)
        .compose(userResults -> {
          if (!userResults.getResults().isEmpty()) {
            var existingUser = userResults.getResults().get(0);
            var updatedUser = updateUserFromStagingUser(stagUser, existingUser, homeAddressTypeId, patronGroupId);
            return conn.upsert(TABLE_NAME_USERS, updatedUser.getId(), updatedUser)
              .compose(res -> {
                log.info("updating existing user {}", res);
                return conn.delete(STAGING_USERS_TABLE, id).map(x -> {
                  log.info("Deletion of staging user with id {} happened successfully", id);
                  return updatedUser;
                });
              });
          } else {
            log.info("Existing user is not found");
            return Future.failedFuture("User not found");
          }
        });
    }
  }

  private User createUserFromStagingUser(StagingUser stagingUser, String homeAddressTypeId, String patronGroupId) {
    User user = new User();
    user.setId(UUID.randomUUID().toString());
    user.setActive(true);
    user.setPatronGroup(patronGroupId);

    Personal personal = createOrUpdatePersonal(stagingUser, new Personal(), homeAddressTypeId);
    user.setPersonal(personal);

    return user;
  }

  private User updateUserFromStagingUser(StagingUser stagingUser, User user, String homeAddressTypeId, String patronGroupId) {
    user.setActive(true);
    user.setPatronGroup(patronGroupId);

    Personal personal = createOrUpdatePersonal(stagingUser, user.getPersonal(), homeAddressTypeId);
    user.setPersonal(personal);

    return user;
  }
  private Personal createOrUpdatePersonal(StagingUser stagingUser, Personal personal, String homeAddressTypeId) {
    var contactInfo = stagingUser.getContactInfo();
    var generalInfo = stagingUser.getGeneralInfo();
    var addressInfo = stagingUser.getAddressInfo();

    if (contactInfo != null) {
      personal.setEmail(contactInfo.getEmail());
      personal.setPhone(contactInfo.getPhone());
      personal.setMobilePhone(contactInfo.getMobilePhone());
      personal.setPreferredContactTypeId("002");
    }
    if (generalInfo != null) {
      personal.setFirstName(generalInfo.getFirstName());
      personal.setLastName(generalInfo.getLastName());
      personal.setMiddleName(generalInfo.getMiddleName());
      personal.setPreferredFirstName(generalInfo.getPreferredFirstName());
    }
    if (addressInfo != null) {
      Address userAddress = personal.getAddresses() != null
        ? personal.getAddresses().stream().filter(address -> homeAddressTypeId.equals(address.getAddressTypeId())).findFirst().orElse(new Address())
        : new Address();

      updateAddress(userAddress, addressInfo, homeAddressTypeId);
      if (personal.getAddresses() == null) {
        personal.setAddresses(List.of(userAddress));
      } else if (!personal.getAddresses().contains(userAddress)) {
        personal.getAddresses().add(userAddress);
      }
    }

    return personal;
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
