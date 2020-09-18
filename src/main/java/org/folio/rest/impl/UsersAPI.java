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

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.mutable.MutableObject;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.User;
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
import io.vertx.core.Promise;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.web.RoutingContext;


/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements Users {

  public static final String TABLE_NAME_USERS = "users";
  public static final String VIEW_NAME_USER_GROUPS_JOIN = "users_groups_view";

  private static final Messages messages = Messages.getInstance();
  private static final Logger logger = LoggerFactory.getLogger(UsersAPI.class);

  /**
   * right now, just query the join view if a cql was passed in, otherwise work with the
   * master users table. this can be optimized in the future to check if there is really a need
   * to use the join view due to cross table cqling - like returning users sorted by group name
   * @param cql
   * @return
   */
  private String getTableName(String cql) {
    if (cql != null && cql.contains("patronGroup.")){
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

  static Response response(String message, Throwable e, String lang,
      Function<String,Response> report400, Function<String,Response> report500) {
    try {
      Throwable cause = e;
      while (cause != null) {
        if (cause instanceof CQLParseException || cause instanceof FieldException) {
          return report400.apply("CQL Parsing Error for '" + message + "': " + cause.getMessage());
        }
        if (cause instanceof IllegalStateException) {
          return report400.apply("CQL Illegal State Error for '" + message + "': " + cause.getMessage());
        }
        cause = cause.getCause();
      }
      return report500.apply(messages.getMessage(lang, MessageConsts.InternalServerError));
    } catch (Exception e2) {
      logger.error(e2.getMessage(), e2);
      return report500.apply(e2.getMessage());
    }
  }

  @Validate
  @Override
  public void getUsers(String query, String orderBy,
    UsersGetOrder order, int offset, int limit, List<String> facets,
    String lang, RoutingContext routingContext, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    try {
      logger.debug("Getting users");
      // note that orderBy is NOT used
      String tableName = getTableName(query);
      CQLWrapper cql = getCQL(query, limit, offset);
      List<FacetField> facetList = FacetManager.convertFacetStrings2FacetFields(facets, "jsonb");

      PgUtil.streamGet(tableName, User.class, cql, facetList, TABLE_NAME_USERS,
        routingContext, okapiHeaders, vertxContext);
    } catch (Exception e) {
      logger.error(query, e);
      Response response = response(query, e, lang,
        GetUsersResponse::respond400WithTextPlain,
        GetUsersResponse::respond500WithTextPlain);
      asyncResultHandler.handle(succeededFuture(response));
    }
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity,
                        RoutingContext routingContext,
                        Map<String, String> okapiHeaders,
                        Handler<AsyncResult<Response>> asyncResultHandler,
                        Context vertxContext) {
    try {
      if (checkForDuplicateAddressTypes(entity)) {
        asyncResultHandler.handle(Future.succeededFuture(
          PostUsersResponse.respond400WithTextPlain(
            "Users are limited to one address per addresstype")));
        return;
      }

      if (StringUtils.isNotBlank(entity.getUsername())) {
        trimWhiteSpaceInUsername(entity);
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
            logger.error(e.getMessage(), e);
            asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.respond500WithTextPlain(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
          return null;
        }).onFailure(e -> {
          logger.error(e.getMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(
            PostUsersResponse.respond500WithTextPlain(e.getMessage())));
        });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
        PostUsersResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  private void saveUser(User entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
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
    return isDesiredError(reply, ".*id.*already exists.*");
  }

  private boolean isDuplicateUsernameError(AsyncResult<Response> reply) {
    return isDesiredError(reply, ".*username.*already exists.*");
  }

  private boolean isDuplicateBarcodeError(AsyncResult<Response> reply) {
    return isDesiredError(reply, ".*barcode.*already exists.*");
  }

  private boolean isDesiredError(AsyncResult<Response> reply, String errMsg) {
    if (reply.succeeded()) {
      if (reply.result().getStatus() == 400) {
        return reply.result().getEntity().toString().matches(errMsg);
  }
      if (reply.result().getStatus() == 422) {
        String msg = ((Errors)reply.result().getEntity()).getErrors().iterator().next().getMessage();
        return msg.matches(errMsg);
  }
    }
    return false;
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
    try {
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
          logger.error(e.getMessage(), e);
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
        }).onFailure(e -> {
          logger.error(e.getMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.respond500WithTextPlain(e.getMessage())));
        });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PutUsersByUserIdResponse.respond500WithTextPlain(e.getMessage())));
    }
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

  private static boolean checkForDuplicateAddressTypes(User user) {
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

  private void trimWhiteSpaceInUsername(User entity) {
    String username = entity.getUsername().trim();
    entity.setUsername(username);
  }

  Future<Boolean> checkAddressTypeValid(
      String addressTypeId, Context vertxContext, PostgresClient postgresClient) {

    Promise<Boolean> promise = Promise.promise();
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
              promise.fail(reply.cause());
            } else {
              List<AddressType> addressTypeList = reply.result().getResults();
              promise.complete(!addressTypeList.isEmpty());
            }
          } catch (Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            promise.fail(e);
          }
        });
      } catch (Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        promise.fail(e);
      }
    });
    return promise.future();
  }

  Future<Boolean> checkAllAddressTypesValid(User user, Context vertxContext, PostgresClient postgresClient) {
    Promise<Boolean> promise = Promise.promise();
    List<Future> futureList = new ArrayList<>();
    if (user.getPersonal() == null || user.getPersonal().getAddresses() == null) {
      promise.complete(true);
      return promise.future();
    }
    for (Address address : user.getPersonal().getAddresses()) {
      String addressTypeId = address.getAddressTypeId();
      Future addressTypeExistsFuture = checkAddressTypeValid(addressTypeId, vertxContext, postgresClient);
      futureList.add(addressTypeExistsFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.onComplete(res -> {
      if (res.failed()) {
        promise.fail(res.cause());
        return;
      }
      for (Future<Boolean> f : futureList) {
        if (Boolean.FALSE.equals(f.result())) {
          promise.complete(false);
          return;
        }
      }
      promise.complete(true);
    });
    return promise.future();
  }

  private static Map<String, Object> getCustomFields(User entity) {
    if (entity.getCustomFields() == null) {
      return Collections.emptyMap();
    }
    return entity.getCustomFields().getAdditionalProperties();
  }
}
