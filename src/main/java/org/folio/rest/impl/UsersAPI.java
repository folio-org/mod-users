package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.mutable.MutableObject;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;
import org.folio.rest.jaxrs.model.UsersGetOrder;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.facets.FacetField;
import org.folio.rest.persist.facets.FacetManager;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import org.folio.validate.CustomFieldValidationException;
import org.folio.validate.ValidationServiceImpl;
import org.z3950.zing.cql.CQLParseException;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;


/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements Users {

  public static final String TABLE_NAME_USERS = "users";
  public static final String VIEW_NAME_USER_GROUPS_JOIN = "users_groups_view";

  private final Messages messages = Messages.getInstance();
  public static final String USER_ID_FIELD = "'id'";
  public static final String USER_NAME_FIELD = "'username'";
  private final Logger logger = LoggerFactory.getLogger(UsersAPI.class);

  /**
   * right now, just query the join view if a cql was passed in, otherwise work with the
   * master users table. this can be optimized in the future to check if there is really a need
   * to use the join view due to cross table cqling - like returning users sorted by group name
   * @param cql
   * @return
   */
  private String getTableName(String cql) {
    if(cql != null && cql.contains("patronGroup.")){
      return VIEW_NAME_USER_GROUPS_JOIN;
    }
    return TABLE_NAME_USERS;
  }

  /**
   * check for entries in the cql which reference the group table, indicating a join is needed
   * and update the cql accordingly - by replacing the patronGroup. prefix with g. which is what
   * the view refers to the groups table
   * @param cql
   * @return
   */
  private static String convertQuery(String cql){
    if (cql != null) {
      return cql.replaceAll("(?i)patronGroup\\.", VIEW_NAME_USER_GROUPS_JOIN+".group_jsonb.");
    }
    return cql;
  }

  static CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException {
    if (query != null && query.contains("patronGroup.")) {
      query = convertQuery(query);
      List<String> fields = new LinkedList<>();
      fields.add(VIEW_NAME_USER_GROUPS_JOIN + ".jsonb");
      fields.add(VIEW_NAME_USER_GROUPS_JOIN + ".group_jsonb");
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(fields);
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    } else {
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(TABLE_NAME_USERS+".jsonb");
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    }
  }

  private void handle(String message, Throwable e, Handler<AsyncResult<Response>> asyncResultHandler, String lang,
      Function<String,Response> report400, Function<String,Response> report500) {
    logger.error(message, e);

    Throwable cause = e;
    do {
      if (cause instanceof CQLParseException || cause instanceof FieldException) {
        asyncResultHandler.handle(Future.succeededFuture(report400.apply(
            "CQL Parsing Error for '" + message + "': " + cause.getMessage())));
        return;
      }
      if (cause instanceof IllegalStateException) {
        asyncResultHandler.handle(Future.succeededFuture(report400.apply(
            "CQL Illegal State Error for '" + message + "': " + cause.getMessage())));
        return;
      }
      cause = cause.getCause();
    } while (cause != null);

    asyncResultHandler.handle(Future.succeededFuture(report500.apply(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
  }

  @Validate
  @Override
  public void getUsers(String query, String orderBy,
      UsersGetOrder order, int offset, int limit, List<String> facets,
      String lang, Map <String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    logger.debug("Getting users");
    try {
      CQLWrapper cql = getCQL(query,limit,offset);

      List<FacetField> facetList = FacetManager.convertFacetStrings2FacetFields(facets, "jsonb");

      String tableName = getTableName(query);
      String[] fieldList = {"*"};
      logger.debug("Headers present are: " + okapiHeaders.keySet().toString());

      PgUtil.postgresClient(vertxContext, okapiHeaders)
          .get(tableName, User.class, fieldList, cql, true, false, facetList, reply -> {
        try {
          if (reply.succeeded()) {
            UserdataCollection userCollection = new UserdataCollection();
            List<User> users = reply.result().getResults();
            userCollection.setUsers(users);
            userCollection.setTotalRecords(reply.result().getResultInfo().getTotalRecords());
            userCollection.setResultInfo(reply.result().getResultInfo());
            asyncResultHandler.handle(Future.succeededFuture(
                GetUsersResponse.respond200WithApplicationJson(userCollection)));
          } else {
            handle(query, reply.cause(), asyncResultHandler, lang,
                GetUsersResponse::respond400WithTextPlain,
                GetUsersResponse::respond500WithTextPlain);
          }
        } catch (Exception e) {
          handle(query, e, asyncResultHandler, lang,
              GetUsersResponse::respond400WithTextPlain,
              GetUsersResponse::respond500WithTextPlain);
        }
      });
    } catch(Exception e) {
      handle(query, e, asyncResultHandler, lang,
          GetUsersResponse::respond400WithTextPlain,
          GetUsersResponse::respond500WithTextPlain);
    }
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity,
                        Map<String, String> okapiHeaders,
                        Handler<AsyncResult<Response>> asyncResultHandler,
                        Context vertxContext) {
    if (checkForDuplicateAddressTypes(entity)) {
      asyncResultHandler.handle(Future.succeededFuture(
        PostUsersResponse.respond400WithTextPlain(
          "Users are limited to one address per addresstype")));
      return;
    }

    MutableObject<PostgresClient> postgresClient = new MutableObject<>();
    succeededFuture()
      .compose(o -> {
        postgresClient.setValue(PgUtil.postgresClient(vertxContext, okapiHeaders));
        return new ValidationServiceImpl(vertxContext)
          .validateCustomFields(getCustomFields(entity), TenantTool.tenantId(okapiHeaders));
      })
      .compose(o -> checkAllAddressTypesValid(entity, vertxContext, postgresClient.getValue()))
      .compose(result -> {
        if (Boolean.FALSE.equals(result)) {
          asyncResultHandler.handle(succeededFuture(
            PostUsersResponse.respond400WithTextPlain(
              "You cannot add addresses with non-existant address types")));
        } else {
          validatePatronGroup(entity.getPatronGroup(), postgresClient.getValue(), asyncResultHandler,
                  handler -> saveUser(entity, okapiHeaders, asyncResultHandler, vertxContext));
        }
        return Future.succeededFuture();
      })
      .otherwise(e -> {
        if (e instanceof CustomFieldValidationException) {
          asyncResultHandler.handle(succeededFuture(
            PostUsersResponse.respond422WithApplicationJson(
              ((CustomFieldValidationException) e).getErrors())));
        } else {
          logger.error(e.getLocalizedMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(
            PostUsersResponse.respond500WithTextPlain(
              messages.getMessage(lang, MessageConsts.InternalServerError))));
        }
        return null;
      });
  }

  private void saveUser(User entity, Map<String, String> okapiHeaders,
                        Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    Date now = new Date();
    entity.setCreatedDate(now);
    entity.setUpdatedDate(now);
    PgUtil.post(TABLE_NAME_USERS, entity, okapiHeaders, vertxContext, PostUsersResponse.class, reply -> {
      if (isDuplicateIdError(reply)) {
        asyncResultHandler.handle(
          succeededFuture(PostUsersResponse.respond422WithApplicationJson(
            ValidationHelper.createValidationErrorMessage(
              "id", entity.getId(),
              "User with this id already exists"))));
        return;
      }
      if (isDuplicateUsernameError(reply)) {
        asyncResultHandler.handle(
          succeededFuture(PostUsersResponse.respond422WithApplicationJson(
            ValidationHelper.createValidationErrorMessage(
              "username", entity.getUsername(),
              "User with this username already exists"))));
        return;
      }
      if (isDuplicateBarcodeError(reply)) {
        asyncResultHandler.handle(
          succeededFuture(PostUsersResponse.respond422WithApplicationJson(
            ValidationHelper.createValidationErrorMessage(
              "barcode", entity.getBarcode(),
              "This barcode has already been taken"))));
        return;
      }
      logger.debug("Save successful");
      asyncResultHandler.handle(reply);
    });
  }

  private boolean isDuplicateIdError(AsyncResult<Response> reply) {
    return reply.succeeded()
    && reply.result().getStatus() == 400
    && reply.result().getEntity().toString().contains("users_pkey");
  }

  private boolean isDuplicateUsernameError(AsyncResult<Response> reply) {
    return reply.succeeded()
    && reply.result().getStatus() == 400
    && reply.result().getEntity().toString().contains("users_username_idx_unique");
  }

  private boolean isDuplicateBarcodeError(AsyncResult<Response> reply) {
    return reply.succeeded()
    && reply.result().getStatus() == 400
    && reply.result().getEntity().toString().contains("users_barcode_idx_unique");
  }

  @Validate
  @Override
  public void getUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    PgUtil.getById(getTableName(null), User.class, userId, okapiHeaders, vertxContext,
      GetUsersByUserIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void deleteUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    PgUtil.deleteById(getTableName(null), userId, okapiHeaders, vertxContext,
      DeleteUsersByUserIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putUsersByUserId(String userId,
          String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    Future.succeededFuture()
      .compose(o -> new ValidationServiceImpl(vertxContext)
        .validateCustomFields(getCustomFields(entity), TenantTool.tenantId(okapiHeaders)))
      .compose(o -> {
        if (checkForDuplicateAddressTypes(entity)) {
          asyncResultHandler.handle(Future.succeededFuture(
            PostUsersResponse.respond400WithTextPlain("Users are limited to one address per addresstype")));
          return Future.succeededFuture();
        }
        if (!userId.equals(entity.getId())) {
          asyncResultHandler.handle(Future.succeededFuture(
            PutUsersByUserIdResponse.respond400WithTextPlain("You cannot change the value of the id field")));
          return Future.succeededFuture();
        }
        PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

        return checkAllAddressTypesValid(entity, vertxContext, postgresClient)
          .compose(result -> {
            if (Boolean.FALSE.equals(result)) {
              asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.respond400WithTextPlain("All addresses types defined for users must be existing")));
            } else {
              validatePatronGroup(entity.getPatronGroup(), postgresClient, asyncResultHandler,
                handler -> updateUser(entity, okapiHeaders, asyncResultHandler, vertxContext));
            }
            return Future.succeededFuture();
          });
      })
      .otherwise(e -> {
        logger.debug(e.getLocalizedMessage());
        if (e instanceof CustomFieldValidationException) {
          asyncResultHandler.handle(succeededFuture(
            PostUsersResponse.respond422WithApplicationJson(
              ((CustomFieldValidationException) e).getErrors())));
        } else {
          asyncResultHandler.handle(Future.succeededFuture(
            PutUsersByUserIdResponse.respond500WithTextPlain(
              messages.getMessage(lang, MessageConsts.InternalServerError))));
        }
        return null;
      });
  }

  private void updateUser(User entity, Map<String, String> okapiHeaders,
                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    Date now = new Date();
    entity.setCreatedDate(now);
    entity.setUpdatedDate(now);
    PgUtil.put(TABLE_NAME_USERS, entity, entity.getId(), okapiHeaders, vertxContext, PutUsersByUserIdResponse.class, reply -> {
      if (isDuplicateUsernameError(reply)) {
        asyncResultHandler.handle(
          succeededFuture(PutUsersByUserIdResponse
            .respond400WithTextPlain(
              "User with this username already exists")));
        return;
      }
      if (isDuplicateBarcodeError(reply)) {
        asyncResultHandler.handle(
          succeededFuture(PutUsersByUserIdResponse
            .respond400WithTextPlain(
              "This barcode has already been taken")));
        return;
      }
      logger.debug("Save successful");
      asyncResultHandler.handle(reply);
    });
  }

  /**
   * Checks if patron group exists
   * @param patronGroupId id of patron group
   * @param postgresClient PostgresClient
   * @param handler Handler that will be called with one of following values
   *                0 if patron group doesn't exist,
   *                1 if patron group exists
   *                -1 if exception was thrown
   */
  private void checkPatronGroupExists(String patronGroupId,
                                      PostgresClient postgresClient,
                                      Handler<AsyncResult<Integer>> handler) {
    if (patronGroupId == null) {
      //allow null patron groups so that they can be added after a record is created
      handler.handle(io.vertx.core.Future.succeededFuture(1));
    } else {
      postgresClient.getById(UserGroupAPI.GROUP_TABLE, patronGroupId, check -> {
        if (check.succeeded()) {
          if (check.result() == null) {
            handler.handle(io.vertx.core.Future.succeededFuture(0));
          } else {
            handler.handle(io.vertx.core.Future.succeededFuture(1));
          }
        } else {
          Throwable t = check.cause();
          logger.error(t.getLocalizedMessage(), t);
          int retCode = -1;
          if (t.getLocalizedMessage().contains("uuid")) {
            retCode = 0;
          }
          handler.handle(io.vertx.core.Future.succeededFuture(retCode));
        }
      });
    }
  }

  /**
   * Validates that patron group with specified id exists
   * @param patronGroupId id of patron group
   * @param postgresClient PostgresClient
   * @param asyncResultHandler handler that will be called with failed Response on validation failure
   * @param onSuccess handler that will be called on validation success
   */
  private void validatePatronGroup(String patronGroupId,
                                   PostgresClient postgresClient,
                                   Handler<AsyncResult<Response>> asyncResultHandler,
                                   Handler<AsyncResult<Void>> onSuccess) {
    checkPatronGroupExists(patronGroupId, postgresClient, handler -> {
      int res = handler.result();
      if (res == 0) {
        String message = "Cannot add " +
          patronGroupId +
          ". Patron group not found";
        logger.error(message);
        asyncResultHandler.handle(Future.succeededFuture(
          PostUsersResponse.respond400WithTextPlain(
            message)));
      } else if (res == -1) {
        asyncResultHandler.handle(Future.succeededFuture(
          PostUsersResponse
            .respond500WithTextPlain("")));
      } else {
        onSuccess.handle(Future.succeededFuture());
      }
    });
  }

  private boolean checkForDuplicateAddressTypes(User user) {
    Map<String, Integer> countMap = new HashMap<>();
    if (user.getPersonal() != null &&
      user.getPersonal().getAddresses() != null) {
      for(Address address : user.getPersonal().getAddresses()) {
        String addressTypeId = address.getAddressTypeId();
        if (addressTypeId != null) {
          boolean found = false;
          for(String key : countMap.keySet()) {
            if (key.equals(addressTypeId)) {
              Integer count = countMap.get(key);
              count = count + 1;
              countMap.put(key, count);
              found = true;
              break;
            }
          }
          if (!found) {
            countMap.put(addressTypeId, 1);
          }
        }
      }
    }
    for(Integer i : countMap.values()) {
      if (i > 1) {
        return true;
      }
    }
    return false;
  }

  private Future<Boolean> checkAddressTypeValid(
      String addressTypeId, Context vertxContext, PostgresClient postgresClient) {

    Future<Boolean> future = Future.future();
    Criterion criterion = new Criterion(
          new Criteria().addField(AddressTypeAPI.ID_FIELD_NAME).
                  setJSONB(false).setOperation("=").setVal(addressTypeId));
    vertxContext.runOnContext(v -> {
      try {
        postgresClient.get(AddressTypeAPI.ADDRESS_TYPE_TABLE, AddressType.class, criterion, true, reply -> {
          try {
            if (reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              future.fail(reply.cause());
            } else {
              List<AddressType> addressTypeList = reply.result().getResults();
              if (addressTypeList.isEmpty()) {
                future.complete(false);
              } else {
                future.complete(true);
              }
            }
          } catch (Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            future.fail(e);
          }
        });
      } catch (Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        future.fail(e);
      }
    });
    return future;
  }

  private Future<Boolean> checkAllAddressTypesValid(User user, Context vertxContext, PostgresClient postgresClient) {
    Future<Boolean> future = Future.future();
    List<Future> futureList = new ArrayList<>();
    if (user.getPersonal() == null || user.getPersonal().getAddresses() == null) {
      future.complete(true);
      return future;
    }
    for (Address address : user.getPersonal().getAddresses()) {
      String addressTypeId = address.getAddressTypeId();
      Future addressTypeExistsFuture = checkAddressTypeValid(addressTypeId, vertxContext, postgresClient);
      futureList.add(addressTypeExistsFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(res -> {
      if (res.failed()) {
        future.fail(res.cause());
      } else {
        boolean bad = false;
        for (Future<Boolean> f : futureList) {
          if (Boolean.FALSE.equals(f.result())) {
            future.complete(false);
            bad = true;
            break;
          }
        }
        if (!bad) {
          future.complete(true);
        }
      }
    });
    return future;
  }

  private Map<String, Object> getCustomFields(User entity) {
    if (entity.getCustomFields() == null) {
      return Collections.emptyMap();
    }
    return entity.getCustomFields().getAdditionalProperties();
  }
}
