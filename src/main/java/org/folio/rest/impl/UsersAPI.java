package org.folio.rest.impl;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.UUID;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import io.vertx.pgclient.PgException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.jaxrs.model.UsersGetOrder;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgExceptionUtil;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import org.folio.rest.utils.ExpirationTool;
import org.folio.event.service.UserOutboxService;
import org.folio.support.FailureHandler;
import org.folio.validate.CustomFieldValidationException;
import org.folio.validate.ValidationServiceImpl;
import org.z3950.zing.cql.CQLParseException;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;

@Path("users")
public class UsersAPI implements Users {

  public static final String TABLE_NAME_USERS = "users";
  public static final Integer DEFAULT_LIMIT = 10000;
  public static final String VIEW_NAME_USER_GROUPS_JOIN = "users_groups_view";

  private static final Messages messages = Messages.getInstance();
  private static final Logger logger = LogManager.getLogger(UsersAPI.class);

  // Used when RMB instantiates this class
  private final UserOutboxService userOutboxService;

  public UsersAPI() {
    this.userOutboxService = new UserOutboxService();
  }
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

  public static CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException {
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
      UsersGetOrder order, int offset, int limit,
      String lang, RoutingContext routingContext, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    try {
      logger.debug("Getting users");
      // note that orderBy is NOT used
      String tableName = getTableName(query);
      CQLWrapper cql = getCQL(query, limit, offset);

      PgUtil.streamGet(tableName, User.class, cql, emptyList(), TABLE_NAME_USERS,
        routingContext, okapiHeaders, vertxContext);
    } catch (Exception e) {
      logger.error(query, e);
      Response response = response(query, e, lang,
        GetUsersResponse::respond400WithTextPlain,
        GetUsersResponse::respond500WithTextPlain);
      asyncResultHandler.handle(succeededFuture(response));
    }
  }

  private void removeCustomFieldIfEmpty(User entity) {
    var customField = (entity.getCustomFields() != null) ?
      entity.getCustomFields().getAdditionalProperties() : null;
    if (customField != null)
      customField.entrySet().removeIf(obj -> {
        if (obj.getValue() instanceof String)
          return obj.getValue().toString().isEmpty();
        return false;
      });
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity, RoutingContext routingContext,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostUsersResponse::respond500WithTextPlain);

    try {
      final var addressValidator = new AddressValidator();

      if (addressValidator.hasMultipleAddressesWithSameType(entity)) {
        asyncResultHandler.handle(succeededFuture(
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
          removeCustomFieldIfEmpty(entity);
          return new ValidationServiceImpl(vertxContext)
            .validateCustomFields(getCustomFields(entity), TenantTool.tenantId(okapiHeaders));
        })
        .compose(o -> checkAllAddressTypesValid(entity, postgresClient.getValue()))
        .compose(result -> {
          if (Boolean.FALSE.equals(result)) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond400WithTextPlain(
                "You cannot add addresses with non-existent address types")));
          } else {
            validatePatronGroup(entity.getPatronGroup(), postgresClient.getValue(), asyncResultHandler,
                    handler -> saveUser(entity, okapiHeaders, postgresClient.getValue(), asyncResultHandler, vertxContext));
          }
          return succeededFuture();
        })
        .otherwise(e -> {
          if (e instanceof CustomFieldValidationException) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond422WithApplicationJson(
                ((CustomFieldValidationException) e).getErrors())));
          } else {
            logger.error(e.getMessage(), e);
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond500WithTextPlain(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
          return null;
        })
        .onFailure(failureHandler::handleFailure);
    } catch (Exception e) {
      failureHandler.handleFailure(e);
    }
  }

  private void saveUser(User entity, Map<String, String> okapiHeaders, PostgresClient pgClient, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    Date now = new Date();
    entity.setCreatedDate(now);
    entity.setUpdatedDate(now);
    String userId = StringUtils.defaultIfBlank(entity.getId(), UUID.randomUUID().toString());

    pgClient.withTrans(conn -> conn.saveAndReturnUpdatedEntity(TABLE_NAME_USERS, userId, entity.withId(userId))
      .compose(user -> userOutboxService.saveUserOutboxLog(conn, user, UserEvent.Action.CREATE, okapiHeaders)
        .map(aVoid -> PostUsersResponse.respond201WithApplicationJson(entity, PostUsersResponse.headersFor201().withLocation(userId)))
        .map(Response.class::cast))
      .onComplete(reply -> {
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
        userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
        asyncResultHandler.handle(reply);
      }));
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
    } else if (reply.cause() instanceof PgException) {
      return PgExceptionUtil.get(reply.cause(), 'D').matches(errMsg);
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

    MutableObject<PostgresClient> postgresClient = new MutableObject<>();
    postgresClient.setValue(PgUtil.postgresClient(vertxContext, okapiHeaders));

    postgresClient.getValue()
      .withTrans(conn -> conn.getById(TABLE_NAME_USERS, userId, User.class)
        .compose(user -> {
          if (Objects.nonNull(user)) {
            PgUtil.deleteById(getTableName(null), userId, okapiHeaders, vertxContext, DeleteUsersByUserIdResponse.class, asyncResultHandler);
            userOutboxService.saveUserOutboxLog(conn, user, UserEvent.Action.DELETE, okapiHeaders);
            userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
          } else {
              asyncResultHandler.handle(
                succeededFuture(DeleteUsersByUserIdResponse.respond404WithTextPlain(
                  ValidationHelper.createValidationErrorMessage("userId", userId,"UserId is not available in system"))));
          }
          return succeededFuture();
        })
      );
  }

  @Validate
  @Override
  public void deleteUsers(String query, RoutingContext routingContext,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    MutableObject<PostgresClient> postgresClient = new MutableObject<>();
    try {
      CQLWrapper wrapper = getCQL(query, DEFAULT_LIMIT, 0);
      postgresClient.setValue(PgUtil.postgresClient(vertxContext, okapiHeaders));
      postgresClient.getValue().withTrans(conn ->
        conn.get(TABLE_NAME_USERS, User.class, wrapper)
          .compose(users -> {
            if(Objects.nonNull(users)) {
              PgUtil.delete(getTableName(null), query, okapiHeaders, vertxContext, DeleteUsersResponse.class, asyncResultHandler);
              userOutboxService.saveUsersListOutboxLog(conn, users.getResults(), UserEvent.Action.DELETE, okapiHeaders);
              userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
            }
            return succeededFuture();
          })
      );
    } catch (CQL2PgJSONException e) {
      throw new IllegalArgumentException(e);
    }
  }

  @Validate
  @Override
  public void putUsersByUserId(String userId,
      String lang, User entity,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PutUsersByUserIdResponse::respond500WithTextPlain);

    try {
      succeededFuture()
        .compose(o -> {
          removeCustomFieldIfEmpty(entity);
          return new ValidationServiceImpl(vertxContext)
              .validateCustomFields(getCustomFields(entity), TenantTool.tenantId(okapiHeaders));
          }
        )
        .compose(o -> {
          final var addressValidator = new AddressValidator();

          if (addressValidator.hasMultipleAddressesWithSameType(entity)) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond400WithTextPlain(
                "Users are limited to one address per addresstype")));
            return succeededFuture();
          }
          if (!userId.equals(entity.getId())) {
            asyncResultHandler.handle(succeededFuture(
              PutUsersByUserIdResponse.respond400WithTextPlain(
                "You cannot change the value of the id field")));
            return succeededFuture();
          }
          PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

          return checkAllAddressTypesValid(entity, postgresClient)
            .compose(result -> {
              if (Boolean.FALSE.equals(result)) {
                asyncResultHandler.handle(succeededFuture(
                  PostUsersResponse.respond400WithTextPlain(
                    "All addresses types defined for users must be existing")));
              } else {
                validatePatronGroup(entity.getPatronGroup(), postgresClient, asyncResultHandler,
                  handler -> updateUser(entity, okapiHeaders, asyncResultHandler, vertxContext));
              }
              return succeededFuture();
            });
        })
        .otherwise(e -> {
          logger.error(e.getMessage(), e);
          if (e instanceof CustomFieldValidationException) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond422WithApplicationJson(
                ((CustomFieldValidationException) e).getErrors())));
          } else {
            asyncResultHandler.handle(succeededFuture(
              PutUsersByUserIdResponse.respond500WithTextPlain(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
          return null;
        })
        .onFailure(failureHandler::handleFailure);
    } catch (Exception e) {
      failureHandler.handleFailure(e);
    }
  }


  @Override
  public void postUsersExpireTimer(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    final var expirationTool = new ExpirationTool();

    expirationTool.doExpirationForTenant(vertxContext.owner(), okapiHeaders.get("x-okapi-tenant"))
        .onSuccess(res -> asyncResultHandler.handle(
            succeededFuture(PostUsersExpireTimerResponse.respond204())))
        .onFailure(cause -> asyncResultHandler.handle(
            succeededFuture(PostUsersExpireTimerResponse.respond500WithTextPlain(cause.getMessage()))));
  }

  @Override
  public void postUsersOutboxProcess(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders)
      .onSuccess(res -> asyncResultHandler.handle(Future.succeededFuture(Response.status(Response.Status.OK).build())))
      .onFailure(cause -> {
        logger.warn("Processing of outbox events table has been failed", cause);
        asyncResultHandler.handle(Future.failedFuture(cause));
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

  private Future<Boolean> patronGroupExists(String patronGroupId,
    PostgresClient postgresClient) {

    if (patronGroupId == null) {
      return succeededFuture(true);
    }

    return postgresClient.getById(UserGroupAPI.GROUP_TABLE, patronGroupId)
      .map(Objects::nonNull);
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

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostUsersResponse::respond500WithTextPlain);

    patronGroupExists(patronGroupId, postgresClient)
      .onSuccess(groupExists -> {
        if (Boolean.TRUE.equals(groupExists)) {
          onSuccess.handle(succeededFuture());
        }
        else {
          String message = "Cannot add " + patronGroupId + ". Patron group not found";
          logger.error(message);
          asyncResultHandler.handle(succeededFuture(
            PostUsersResponse.respond400WithTextPlain(message)));
        }
      })
      .onFailure(failureHandler::handleFailure);
  }

  private void trimWhiteSpaceInUsername(User entity) {
    String username = entity.getUsername().trim();
    entity.setUsername(username);
  }

  Future<Boolean> checkAddressTypeValid(String addressTypeId, PostgresClient postgresClient) {
    final var criterion = new Criterion(
      new Criteria().addField(AddressTypeAPI.ID_FIELD_NAME)
        .setJSONB(false).setOperation("=").setVal(addressTypeId));

    try {
      return postgresClient.get(AddressTypeAPI.ADDRESS_TYPE_TABLE, AddressType.class, criterion, true)
        .map(addressTypes -> !addressTypes.getResults().isEmpty());
    }
    catch (Exception e) {
      return failedFuture(e);
    }
  }

  Future<Boolean> checkAllAddressTypesValid(User user, PostgresClient postgresClient) {
    List<Future<Boolean>> futureList = new ArrayList<>();

    if (user.getPersonal() == null || user.getPersonal().getAddresses() == null) {
      return succeededFuture(true);
    }

    final var addressTypes = user.getPersonal().getAddresses()
      .stream()
      .map(Address::getAddressTypeId)
      .collect(Collectors.toList());

    addressTypes.forEach(addressTypeId -> futureList.add(
      checkAddressTypeValid(addressTypeId, postgresClient)));

    return GenericCompositeFuture.all(futureList)
      .map(res -> futureList.stream()
        .map(Future::result)
        .allMatch(Predicate.isEqual(true)));
  }

  private static Map<String, Object> getCustomFields(User entity) {
    if (entity.getCustomFields() == null) {
      return Collections.emptyMap();
    }
    return entity.getCustomFields().getAdditionalProperties();
  }
}
