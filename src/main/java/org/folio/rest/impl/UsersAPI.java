package org.folio.rest.impl;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.emptyList;
import static org.apache.commons.io.FileUtils.ONE_MB;
import static org.folio.domain.UserType.SHADOW;
import static org.folio.event.service.UserTenantService.INVALID_USER_TYPE_POPULATED;
import static org.folio.event.service.UserTenantService.USERNAME_IS_NOT_POPULATED;
import static org.folio.rest.RestVerticle.STREAM_ABORT;
import static org.folio.rest.RestVerticle.STREAM_COMPLETE;
import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;
import static org.folio.service.event.EntityChangedEventPublisherFactory.userEventPublisher;
import static org.folio.service.storage.ProfilePictureStorage.createSelectQuery;
import static org.folio.support.UsersApiConstants.BARCODE_ALREADY_EXISTS;
import static org.folio.support.UsersApiConstants.CONFIG_NAME;
import static org.folio.support.UsersApiConstants.DELETE_PROFILE_PICTURE_SQL;
import static org.folio.support.UsersApiConstants.DELETE_USERS_SQL;
import static org.folio.support.UsersApiConstants.DUPLICATE_BARCODE_ERROR;
import static org.folio.support.UsersApiConstants.DUPLICATE_ID_ERROR;
import static org.folio.support.UsersApiConstants.DUPLICATE_USERNAME_ERROR;
import static org.folio.support.UsersApiConstants.ENABLED;
import static org.folio.support.UsersApiConstants.ENABLED_OBJECT_STORAGE;
import static org.folio.support.UsersApiConstants.ENCRYPTION_KEY;
import static org.folio.support.UsersApiConstants.GET_CONFIG_SQL;
import static org.folio.support.UsersApiConstants.ID;
import static org.folio.support.UsersApiConstants.INVALID_USERNAME_ERROR;
import static org.folio.support.UsersApiConstants.INVALID_USER_TYPE_ERROR;
import static org.folio.support.UsersApiConstants.JSONB;
import static org.folio.support.UsersApiConstants.KEY_ERROR;
import static org.folio.support.UsersApiConstants.MAX_DOCUMENT_SIZE;
import static org.folio.support.UsersApiConstants.MAX_FILE_SIZE;
import static org.folio.support.UsersApiConstants.PROFILE_PICTURE_FOR_SHADOW_USER_ERROR_MSG;
import static org.folio.support.UsersApiConstants.RETURNING_USERS_ID_SQL;
import static org.folio.support.UsersApiConstants.TABLE_NAME_CONFIG;
import static org.folio.support.UsersApiConstants.TABLE_NAME_PROFILE_PICTURE;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;
import static org.folio.support.UsersApiConstants.USERNAME_ALREADY_EXISTS;
import static org.folio.support.UsersApiConstants.VIEW_NAME_USER_GROUPS_JOIN;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;
import io.vertx.pgclient.PgException;

import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.client.impl.FeesFinesModuleClientImpl;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.cql2pgjson.exception.QueryValidationException;
import org.folio.domain.UserType;
import org.folio.event.service.UserTenantService;
import org.folio.integration.http.HttpClientFactory;
import org.folio.integration.http.VertxOkapiHttpClient;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.annotations.Stream;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserEvent;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgExceptionUtil;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLQueryValidationException;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import org.folio.rest.utils.ExpirationTool;
import org.folio.event.service.UserOutboxService;
import org.folio.service.UsersService;
import org.folio.service.storage.ProfilePictureStorage;
import org.folio.support.FailureHandler;
import org.folio.support.ProfilePictureHelper;
import org.folio.validate.CustomFieldValidationException;
import org.folio.validate.ValidationServiceImpl;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.z3950.zing.cql.CQLParseException;

@Path("users")
public class UsersAPI implements Users {

  private static final Logger logger = LogManager.getLogger(UsersAPI.class);
  private static final String KEYWORDS_TSVECTOR = getKeywordsTsVector();
  private static final String USERS_JSONB = TABLE_NAME_USERS + ".jsonb";
  @SuppressWarnings("deprecation")  // RAML requires Date
  private static final Date year1 = new Date(1 - 1900, 0 /* 0 .. 11 */, 1 /* 1 .. 31 */);
  private byte[] requestBytesArray = new byte[0];

  // Used when RMB instantiates this class
  private final UserOutboxService userOutboxService;
  private final UsersService usersService;
  private final UserTenantService userTenantService;
  private final ProfilePictureStorage profilePictureStorage;

  @Autowired
  public UsersAPI() {
    this.profilePictureStorage = new ProfilePictureStorage();
    this.userOutboxService = new UserOutboxService();
    this.usersService = new UsersService();
    this.userTenantService = new UserTenantService();
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

  private static String getKeywordsTsVector() {
    try {
      var sql = new CQL2PgJSON(USERS_JSONB).toSql("keywords=foo").getWhere();
      return sql.substring(0, sql.indexOf(" @@ "))
          .replace("users.jsonb->", "users_groups_view.jsonb->");
    } catch (FieldException | QueryValidationException e) {
      throw new CQLQueryValidationException(e);
    }
  }

  static class ViewCqlWrapper extends CQLWrapper {
    public ViewCqlWrapper(CQL2PgJSON field, String query, int limit, int offset) {
      super(field, query, limit, offset);
    }

    @Override
    public String getWhereClause() {
      return super.getWhereClause().replace(
          "get_tsvector(f_unaccent(users_groups_view.jsonb->>'keywords'))", KEYWORDS_TSVECTOR);
    }
  }

  public static CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException {
    if (query != null && query.contains("patronGroup.")) {
      query = convertQuery(query);
      List<String> fields = new LinkedList<>();
      fields.add(VIEW_NAME_USER_GROUPS_JOIN + ".jsonb");
      fields.add(VIEW_NAME_USER_GROUPS_JOIN + ".group_jsonb");
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(fields);
      return new ViewCqlWrapper(cql2pgJson, query, limit, offset);
    } else {
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(USERS_JSONB);
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    }
  }

  static Response response(String message, Throwable e,
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
      return report500.apply(e != null ? e.getMessage() : "Internal Server Error");
    } catch (Exception e2) {
      logger.error(e2.getMessage(), e2);
      return report500.apply(e2.getMessage());
    }
  }

  @Validate
  @Override
  public void getUsers(String query, String totalRecords, int offset, int limit,
    RoutingContext routingContext, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    try {
      logger.debug("Getting users");
      // note that orderBy is NOT used
      String tableName = getTableName(query);
      CQLWrapper cql = getCQL(query, limit, offset);

      PgUtil.streamGet(tableName, User.class, cql, emptyList(), TABLE_NAME_USERS,
        routingContext, okapiHeaders, vertxContext);
    } catch (Exception e) {
      logger.error(query, e);
      Response response = response(query, e,
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
  public void postUsers(User entity, RoutingContext routingContext,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostUsersResponse::respond500WithTextPlain);

    if (isProfilePictureLinkPresentForShadow(entity)) {
      asyncResultHandler.handle(succeededFuture(PostUsersResponse.respond500WithTextPlain(PROFILE_PICTURE_FOR_SHADOW_USER_ERROR_MSG)));
      return;
    }

    try {
      var dateOfBirthError = validateDateOfBirth(entity);
      if (dateOfBirthError != null) {
        asyncResultHandler.handle(succeededFuture(
            PostUsersResponse.respond400WithTextPlain(dateOfBirthError)));
        return;
      }

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
          if (e instanceof CustomFieldValidationException customFieldValidationException) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond422WithApplicationJson(
                customFieldValidationException.getErrors())));
          } else {
            logger.error(e.getMessage(), e);
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond500WithTextPlain(e.getMessage())));
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

    pgClient.withTrans(conn -> userTenantService.validateUserAcrossTenants(entity, okapiHeaders, conn, vertxContext)
        .compose(aVoid -> conn.saveAndReturnUpdatedEntity(TABLE_NAME_USERS, userId, entity.withId(userId))
          .onSuccess(updatedUser -> userEventPublisher(vertxContext, okapiHeaders)
            .publishCreated(entity.getId(), updatedUser))
          .compose(user -> userOutboxService.saveUserOutboxLogForCreateUser(conn, user, UserEvent.Action.CREATE, okapiHeaders))
          .map(isUserOutboxLogCreated -> PostUsersResponse.respond201WithApplicationJson(entity, PostUsersResponse.headersFor201().withLocation(userId)))
          .map(Response.class::cast)))

      .onComplete(reply -> {
        if (reply.succeeded()) {
          logger.debug("Save successful");
          userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
          asyncResultHandler.handle(reply);
          return;
        }

        if (isDuplicateIdError(reply)) {
          logger.warn("User with id {} already exists", entity.getId());
          asyncResultHandler.handle(
            succeededFuture(PostUsersResponse.respond422WithApplicationJson(
              ValidationHelper.createValidationErrorMessage(
                "id", entity.getId(), DUPLICATE_ID_ERROR))));
          return;
        }
        if (isDuplicateUsernameError(reply)) {
          logger.warn("User with this username {} already exists", entity.getUsername());
          asyncResultHandler.handle(
            succeededFuture(PostUsersResponse.respond422WithApplicationJson(
              ValidationHelper.createValidationErrorMessage(
                "username", entity.getUsername(), DUPLICATE_USERNAME_ERROR))));
          return;
        }
        if (isDuplicateBarcodeError(reply)) {
          logger.warn("This barcode {} has already been taken", entity.getBarcode());
          asyncResultHandler.handle(
            succeededFuture(PostUsersResponse.respond422WithApplicationJson(
              ValidationHelper.createValidationErrorMessage(
                "barcode", entity.getBarcode(), DUPLICATE_BARCODE_ERROR))));
          return;
        }
        if (isInvalidUserTypeError(reply)) {
          logger.warn("An invalid user type {} has been populated to a user with id {}", entity.getType(), entity.getId());
          asyncResultHandler.handle(
            succeededFuture(PostUsersResponse.respond422WithApplicationJson(
              ValidationHelper.createValidationErrorMessage(
                "id", entity.getId(),
                String.format(INVALID_USER_TYPE_ERROR, Arrays.stream(UserType.values()).map(UserType::getTypeName).toList())))));
          return;
        }
        if (isInvalidUsernameError(reply)) {
          logger.warn("The user with the ID {} must have a username since consortium mode is enabled", entity.getId());
          asyncResultHandler.handle(
            succeededFuture(PostUsersResponse.respond400WithTextPlain(String.format(INVALID_USERNAME_ERROR, entity.getId()))));
          return;
        }

        logger.error("saveUser failed: {}", reply.cause().getMessage(), reply.cause());
        ValidationHelper.handleError(reply.cause(), asyncResultHandler);
      });
  }

  private boolean isDuplicateIdError(AsyncResult<Response> reply) {
    return isDesiredError(reply, ".*id.*already exists.*");
  }

  private boolean isDuplicateUsernameError(AsyncResult<Response> reply) {
    return isDesiredError(reply, ".*username.*already exists.*");
  }

  private boolean isDuplicateUsernameError(String errorMessage) {
    return errorMessage.contains(USERNAME_ALREADY_EXISTS);
  }

  private boolean isDuplicateBarcodeError(String errorMessage) {
    return errorMessage.contains(BARCODE_ALREADY_EXISTS);
  }

  private boolean isDuplicateBarcodeError(AsyncResult<Response> reply) {
    return isDesiredError(reply, ".*barcode.*already exists.*");
  }

  private boolean isInvalidUserTypeError(String errorMessage) {
    return errorMessage.matches(INVALID_USER_TYPE_POPULATED);
  }

  private boolean isInvalidUserTypeError(AsyncResult<Response> reply) {
    return isDesiredError(reply, INVALID_USER_TYPE_POPULATED);
  }

  private boolean isInvalidUsernameError(String errorMessage) {
    return errorMessage.matches(USERNAME_IS_NOT_POPULATED);
  }

  private boolean isInvalidUsernameError(AsyncResult<Response> reply) {
    return isDesiredError(reply, USERNAME_IS_NOT_POPULATED);
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
    } else if (Objects.nonNull(reply.cause()) && StringUtils.isNotEmpty(reply.cause().getMessage())) {
      return reply.cause().getMessage().matches(errMsg);
    }
    return false;
  }

  @Validate
  @Override
  public void getUsersByUserId(String userId,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    PgUtil.getById(getTableName(null), User.class, userId, okapiHeaders, vertxContext,
      GetUsersByUserIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void deleteUsersByUserId(String userId,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    PgUtil.postgresClient(vertxContext, okapiHeaders)
      .withTrans(conn -> usersService.getUserByIdForUpdate(conn, userId).compose(user -> {
        if (user == null) {
          return succeededFuture(DeleteUsersByUserIdResponse.respond404WithTextPlain(userId));
        }
        // Delete user first within the transaction
        return conn.delete(TABLE_NAME_USERS, userId)
          .compose(rows -> {
            userEventPublisher(vertxContext, okapiHeaders).publishRemoved(userId, user);
            if (rows.rowCount() != 0) {
              return userOutboxService.saveUserOutboxLogForDeleteUser(conn, new User().withId(userId),
                  UserEvent.Action.DELETE, okapiHeaders)
                .map(bVoid -> DeleteUsersByUserIdResponse.respond204())
                .map(Response.class::cast);
            } else {
              return succeededFuture(DeleteUsersByUserIdResponse.respond404WithTextPlain(userId));
            }
          });
      }))
      .map(response ->
        // After transaction completes successfully, delete manual blocks
        removeManualPatronBlocks(userId, okapiHeaders, vertxContext, response)
      )
      .onComplete(reply -> {
        userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
        asyncResultHandler.handle(reply);
      });
  }

  private @NotNull Response removeManualPatronBlocks(String userId, Map<String, String> okapiHeaders, Context vertxContext, Response response) {
    if (response.getStatus() == Response.Status.NO_CONTENT.getStatusCode()) { // Only if user was successfully deleted
      FeesFinesModuleClientImpl feesFinesClient = getFeesFinesModuleClient(vertxContext);
      feesFinesClient.deleteManualBlocksByUserId(userId, okapiHeaders)
        .onFailure(throwable -> logger.warn("Failed to delete manual blocks for user {}: {}", userId, throwable.getMessage()));
    }
    return response;
  }

  private @NotNull FeesFinesModuleClientImpl getFeesFinesModuleClient(Context vertxContext) {
    VertxOkapiHttpClient httpClient = HttpClientFactory.getHttpClient(vertxContext.owner());
    return new FeesFinesModuleClientImpl(httpClient);
  }

  @Validate
  @Override
  public void deleteUsers(String query, RoutingContext routingContext,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    try {
      if (StringUtils.isBlank(query)) {
        var msg = "Expected CQL but query parameter is empty";
        asyncResultHandler.handle(Future.succeededFuture(DeleteUsersResponse.respond400WithTextPlain(msg)));
        return;
      }
      CQLWrapper wrapper = getCQL(query, -1, -1);
      PgUtil.postgresClient(vertxContext, okapiHeaders).withTrans(conn -> conn.execute(createDeleteQuery(wrapper, okapiHeaders))
        .compose(rows -> {
          if (rows.rowCount() != 0) {
            List<User> users = new ArrayList<>();
            rows.iterator().forEachRemaining(row -> users.add(new User().withId(row.getUUID(ID).toString())));
            return userOutboxService.saveUserOutboxLogForDeleteUsers(conn, users, okapiHeaders);
          }
          return Future.succeededFuture();
        }))
        .map(bVoid -> DeleteUsersByUserIdResponse.respond204())
        .map(Response.class::cast)
        .onComplete(reply -> {
          userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
          asyncResultHandler.handle(reply);
        });
    } catch (CQL2PgJSONException e) {
      throw new IllegalArgumentException("Invalid query", e);
    }
  }

  @Validate
  @Override
  public void putUsersByUserId(String userId, User entity,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PutUsersByUserIdResponse::respond500WithTextPlain);

    if (isProfilePictureLinkPresentForShadow(entity)) {
      asyncResultHandler.handle(succeededFuture(PutUsersByUserIdResponse.respond500WithTextPlain(PROFILE_PICTURE_FOR_SHADOW_USER_ERROR_MSG)));
      return;
    }

    try {
      var dateOfBirthError = validateDateOfBirth(entity);
      if (dateOfBirthError != null) {
        asyncResultHandler.handle(succeededFuture(
            PutUsersByUserIdResponse.respond400WithTextPlain(dateOfBirthError)));
        return;
      }

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
                  handler -> updateUser(entity, okapiHeaders, postgresClient, asyncResultHandler, vertxContext));
              }
              return succeededFuture();
            });
        })
        .otherwise(e -> {
          logger.error(e.getMessage(), e);
          if (e instanceof CustomFieldValidationException customFieldValidationException) {
            asyncResultHandler.handle(succeededFuture(
              PostUsersResponse.respond422WithApplicationJson(
                customFieldValidationException.getErrors())));
          } else {
            asyncResultHandler.handle(succeededFuture(
              PutUsersByUserIdResponse.respond500WithTextPlain(e.getMessage())));
          }
          return null;
        })
        .onFailure(failureHandler::handleFailure);
    } catch (Exception e) {
      failureHandler.handleFailure(e);
    }
  }

  private boolean isProfilePictureLinkPresentForShadow(User entity) {
    if (Objects.nonNull(entity) && Objects.nonNull(entity.getPersonal()) && Objects.nonNull(entity.getType())) {
      return entity.getType().equals(SHADOW.getTypeName()) && Objects.nonNull(entity.getPersonal().getProfilePictureLink());
    }
    return false;
  }

  @Override
  public void postUsersExpireTimer(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
    final var expirationTool = new ExpirationTool();
    expirationTool.doExpirationForTenant(vertxContext, okapiHeaders)
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

  @Stream
  @Override
  public void postUsersProfilePicture(InputStream entity, Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try (InputStream bis = new BufferedInputStream(entity)) {
      if (Objects.isNull(okapiHeaders.get(STREAM_COMPLETE))) {
        validateAndProcessByteArray(bis);
      } else if (Objects.nonNull(okapiHeaders.get(STREAM_ABORT))) {
        logger.error("postUsersProfilePicture:: Stream aborted");
        handleStreamAbort(asyncResultHandler);
      } else {
        processProfilePicture(okapiHeaders, asyncResultHandler, vertxContext);
      }
    } catch (Exception e) {
      logger.error("postUsersProfilePicture:: failed to save profile picture due to %s", e.getCause());
      handleException(asyncResultHandler, e);
    }
  }

  private void processProfilePicture(Map<String, String> okapiHeaders,
                                     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(requestBytesArray) && requestBytesArray.length != 0) {
      processValidProfilePicture(okapiHeaders, asyncResultHandler, vertxContext);
    } else {
      handleInvalidProfilePictureSize(asyncResultHandler);
    }
  }

  private void processValidProfilePicture(Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (ProfilePictureHelper.detectFileType(requestBytesArray).equals("Unknown")) {
      logger.error("processValidProfilePicture:: Unknown type received");
      handleInvalidFileType(asyncResultHandler);
    } else {
      processProfilePictureStorage(okapiHeaders, asyncResultHandler, vertxContext);
    }
  }

  private void processProfilePictureStorage(Map<String, String> okapiHeaders,
                                            Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    profilePictureStorage.getProfilePictureConfig(okapiHeaders, vertxContext)
      .onSuccess(config ->
        handleProfilePictureConfig(config, okapiHeaders, asyncResultHandler, vertxContext));
  }

  private void handleProfilePictureConfig(Config config, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(config) && Boolean.FALSE.equals(config.getEnabled())) {
      logger.info("handleProfilePictureConfig:: Profile picture feature is disable");
      handleDisabledProfilePictureFeature(okapiHeaders, asyncResultHandler);
    } else if (Objects.nonNull(config) && Objects.nonNull(config.getMaxFileSize()) && requestBytesArray.length > (config.getMaxFileSize() * ONE_MB)) {
      logger.info("handleProfilePictureConfig:: Validating file size");
      handleInvalidProfilePictureSize(asyncResultHandler);
    } else if (Objects.nonNull(config) && Boolean.TRUE.equals(config.getEnabledObjectStorage())) {
      logger.info("handleProfilePictureConfig:: Storing images into Object storage");
      profilePictureStorage.storeProfilePictureInObjectStorage(requestBytesArray, okapiHeaders, null, asyncResultHandler);
    } else {
      if (config != null && config.getEncryptionKey() != null) {
        logger.info("handleProfilePictureConfig:: Storing images into DB storage");
        profilePictureStorage.storeProfilePictureInDbStorage(requestBytesArray, okapiHeaders, asyncResultHandler, config.getEncryptionKey(), vertxContext);
      } else {
        logger.error("handleProfilePictureConfig:: Encryption key is null");
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse
          .respond500WithApplicationJson(KEY_ERROR)));
      }
    }
  }

  private void handleInvalidFileType(Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(
      succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson("Requested image should be of supported type-[PNG,JPG,JPEG]")));
  }

  private void handleInvalidProfilePictureSize(Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(
      succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson("Requested file size should be within allowed size updated in profile_picture configuration")));
  }

  private void handleStreamAbort(Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson("Stream aborted")));
  }

  private void handleException(Handler<AsyncResult<Response>> asyncResultHandler, Exception e) {
    asyncResultHandler.handle(
      succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson("failed to save profile picture " + e.getMessage())));
  }

  private void validateAndProcessByteArray(InputStream is) throws IOException {
    if (Objects.nonNull(requestBytesArray) && requestBytesArray.length < MAX_DOCUMENT_SIZE && is.available() < MAX_DOCUMENT_SIZE) {
      requestBytesArray = ArrayUtils.addAll(requestBytesArray, IOUtils.toByteArray(is));
    } else {
      requestBytesArray = null;
    }
  }

  @Override
  public void getUsersProfilePictureByProfileId(String profileId, Map<String, String> okapiHeaders,
                                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    profilePictureStorage.getProfilePictureConfig(okapiHeaders, vertxContext)
      .onSuccess(config -> handleProfilePictureConfig(config, profileId, okapiHeaders, asyncResultHandler, vertxContext))
      .onFailure(throwable -> handleProfileConfigFailure(asyncResultHandler));
  }

  private void handleProfilePictureConfig(Config config, String profileId, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(config) && Boolean.FALSE.equals(config.getEnabled())) {
      logger.error("handleProfilePictureConfig:: Profile Picture feature is not enabled");
      handleDisabledProfilePictureFeature(okapiHeaders, asyncResultHandler);
    } else if (Objects.nonNull(config) && Boolean.TRUE.equals(config.getEnabledObjectStorage())) {
      logger.info("handleProfilePictureConfig:: Getting image from object storage");
      profilePictureStorage.getProfilePictureFromObjectStorage(profileId, asyncResultHandler, okapiHeaders);
    } else {
      if (config != null && config.getEncryptionKey() != null) {
        logger.info("handleProfilePictureConfig:: Getting image from DB storage");
        profilePictureStorage.getProfilePictureFromDbStorage(profileId, asyncResultHandler, okapiHeaders, config.getEncryptionKey(), vertxContext);
      } else {
        logger.error("handleProfilePictureConfig:: Encryption key is null");
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse
          .respond500WithApplicationJson("Encryption key is null")));
      }
    }
  }

  private void handleProfileConfigFailure(Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(
      succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson("Failed to retrieve profile picture configuration")));
  }

  private void handleDisabledProfilePictureFeature(Map<String, String> okapiHeaders,
                                                   Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(
      succeededFuture(PostUsersProfilePictureResponse.respond500WithApplicationJson(String.format("Profile picture feature is not enabled for tenant %s", TenantTool.tenantId(okapiHeaders)))));
  }

  @Stream
  @Override
  public void putUsersProfilePictureByProfileId(String profileId, InputStream entity, Map<String, String> okapiHeaders,
                                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try (InputStream bis = new BufferedInputStream(entity)) {
      if (Objects.isNull(okapiHeaders.get(STREAM_COMPLETE))) {
        validateAndProcessByteArray(bis);
      } else if (Objects.nonNull(okapiHeaders.get(STREAM_ABORT))) {
        handleStreamAbort(asyncResultHandler);
      } else {
        processPutProfilePicture(okapiHeaders, profileId, asyncResultHandler, vertxContext);
      }
    } catch (Exception e) {
      handleException(asyncResultHandler, e);
    }
  }

  private void processPutProfilePicture(Map<String, String> okapiHeaders, String profileId,
                                        Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(requestBytesArray) && requestBytesArray.length != 0) {
      if (ProfilePictureHelper.detectFileType(requestBytesArray).equals("Unknown")) {
        handleInvalidFileType(asyncResultHandler);
      } else {
        processPutProfilePictureStorage(okapiHeaders, profileId, asyncResultHandler, vertxContext);
      }
    } else {
      handleInvalidProfilePictureSize(asyncResultHandler);
    }
  }

  private void processPutProfilePictureStorage(Map<String, String> okapiHeaders, String profileId,
                                            Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    profilePictureStorage.getProfilePictureConfig(okapiHeaders, vertxContext)
      .onSuccess(config ->
        handlePutProfilePictureConfig(config, okapiHeaders, profileId, asyncResultHandler, vertxContext));
  }

  private void handlePutProfilePictureConfig(Config config, Map<String, String> okapiHeaders, String profileId,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(config) && Boolean.FALSE.equals(config.getEnabled())) {
      logger.info("handleProfilePictureConfig:: Profile picture feature is disabled");
      handleDisabledProfilePictureFeature(okapiHeaders, asyncResultHandler);
    } else if (Objects.nonNull(config) && Objects.nonNull(config.getMaxFileSize()) && requestBytesArray.length > (config.getMaxFileSize() * ONE_MB)) {
      logger.info("handlePutProfilePictureConfig:: Validating file size");
      handleInvalidProfilePictureSize(asyncResultHandler);
    } else if (Objects.nonNull(config) && Boolean.TRUE.equals(config.getEnabledObjectStorage())) {
      logger.info("handleProfilePictureConfig:: Updating image data into Object storage for id {}", profileId);
      profilePictureStorage.storeProfilePictureInObjectStorage(requestBytesArray, okapiHeaders, profileId, asyncResultHandler);
    } else {
      if (config != null && config.getEncryptionKey() != null) {
        logger.info("handleProfilePictureConfig:: Updating image data into DB storage for id {}", profileId);
        profilePictureStorage.updateProfilePictureInDbStorage(profileId, requestBytesArray, asyncResultHandler, okapiHeaders, config.getEncryptionKey(), vertxContext);
      } else {
        logger.error("handlePutProfilePictureConfig:: Encryption key is null");
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse
          .respond500WithApplicationJson("Encryption key is null")));
      }
    }
  }

  @Override
  public void deleteUsersProfilePictureByProfileId(String profileId, Map<String, String> okapiHeaders,
                                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("deleteUsersProfilePictureByProfileId:: Deleting profile picture with id {} ", profileId);
    profilePictureStorage.getProfilePictureConfig(okapiHeaders, vertxContext)
      .onSuccess(config -> handleProfilePictureConfigForDelete(config, okapiHeaders, profileId, asyncResultHandler, vertxContext))
      .onFailure(throwable -> handleProfileConfigFailure(asyncResultHandler));
  }

  @Override
  public void postUsersProfilePictureCleanup(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("postUsersProfilePictureCleanup:: CleanUp job starting..");
    profilePictureStorage.cleanUp(okapiHeaders, vertxContext)
      .onSuccess(res -> asyncResultHandler.handle(Future.succeededFuture(Response.status(Response.Status.OK).build())))
      .onFailure(cause -> {
        logger.warn("postUsersProfilePictureCleanup:: CleanUp job has been failed", cause);
        asyncResultHandler.handle(Future.failedFuture(cause));
      });
  }

  private void handleProfilePictureConfigForDelete(Config config, Map<String, String> okapiHeaders, String profileId,
                                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (Objects.nonNull(config) && Boolean.FALSE.equals(config.getEnabled())) {
      logger.info("handleProfilePictureConfigForDelete:: Profile picture feature is disabled");
      handleDisabledProfilePictureFeature(okapiHeaders, asyncResultHandler);
    } else if (Objects.nonNull(config) && Boolean.TRUE.equals(config.getEnabledObjectStorage())) {
      logger.info("handleProfilePictureConfigForDelete:: Removing image data into Object storage for id {}", profileId);
      profilePictureStorage.removeProfilePictureFromObjectStorage(okapiHeaders, profileId, asyncResultHandler);
    } else {
      logger.info("handleProfilePictureConfigForDelete:: Removing image data into DB storage for id {}", profileId);
      deleteProfilePictureFromStorage(okapiHeaders, profileId, asyncResultHandler, vertxContext);
    }
  }

  private void deleteProfilePictureFromStorage(Map<String, String> okapiHeaders, String profileId,
                                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createDeleteQuery(okapiHeaders), Tuple.of(profileId))
      .compose(this::handleDeleteResult)
      .onComplete(reply -> handleDeleteCompletion(reply, asyncResultHandler));
  }

  private Future<Response> handleDeleteResult(RowSet<Row> rows) {
    if (rows.rowCount() != 0) {
      logger.info("handleDeleteResult:: Profile picture removed successfully");
      return succeededFuture(DeleteUsersProfilePictureByProfileIdResponse.respond204());
    } else {
      return succeededFuture(DeleteUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("Profile picture not found"));
    }
  }

  private void handleDeleteCompletion(AsyncResult<Response> reply, Handler<AsyncResult<Response>> asyncResultHandler) {
    if (reply.cause() != null) {
      asyncResultHandler.handle(succeededFuture(DeleteUsersProfilePictureByProfileIdResponse.respond500WithApplicationJson(reply.cause())));
    }
    asyncResultHandler.handle(reply);
  }

  @Override
  public void getUsersConfigurationsEntry(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("getUsersConfigurationsEntry:: Getting configuration");
    PgUtil.postgresClient(vertxContext, okapiHeaders).execute(createSelectQuery(okapiHeaders, GET_CONFIG_SQL, TABLE_NAME_CONFIG))
      .compose(rows -> {
        if (rows.rowCount() != 0) {
          return succeededFuture(Users.GetUsersConfigurationsEntryResponse.respond200WithApplicationJson(mapResultSetToConfig(rows)));
        } else {
          return succeededFuture(Users.GetUsersConfigurationsEntryResponse.respond404WithTextPlain("No configuration exist"));
        }
      })
      .map(Response.class::cast)
      .onComplete(responseAsyncResult -> {
        if (responseAsyncResult.cause() != null) {
          logger.error("getUsersConfigurationsEntry:: Can not get profile picture configuration");
          asyncResultHandler.handle(
            succeededFuture(Users.GetUsersConfigurationsEntryResponse
              .respond400WithTextPlain(responseAsyncResult.cause().getMessage())));
        }
        asyncResultHandler.handle(responseAsyncResult);
      });
  }

  @Override
  public void putUsersConfigurationsEntryByConfigId(String configId, Config entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("putUsersConfigurationsEntryByConfigId:: Updating configuration");
    profilePictureStorage.getProfilePictureConfig(okapiHeaders, vertxContext)
      .onSuccess(config -> {
        if (Objects.isNull(entity.getEncryptionKey()) || !Objects.equals(config.getEncryptionKey(), entity.getEncryptionKey())) {
          asyncResultHandler.handle(succeededFuture(Users.PutUsersConfigurationsEntryByConfigIdResponse.respond400WithTextPlain("Cannot update the Encryption key")));
          return;
        }
        if (Objects.nonNull(entity.getMaxFileSize()) && entity.getMaxFileSize() > 10) {
          asyncResultHandler.handle(succeededFuture(Users.PutUsersConfigurationsEntryByConfigIdResponse.respond500WithTextPlain("Max file size should not exceed more than 10 megabytes")));
          return;
        }
        PgUtil.put(TABLE_NAME_CONFIG, entity, configId, okapiHeaders, vertxContext, PutUsersConfigurationsEntryByConfigIdResponse.class, asyncResultHandler);
      })
      .onFailure(throwable -> asyncResultHandler.handle(succeededFuture(PutUsersConfigurationsEntryByConfigIdResponse.respond500WithTextPlain("Failed to retrieve profile picture config"))));
  }

  private void updateUser(User entity, Map<String, String> okapiHeaders, PostgresClient pgClient,
                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    Date now = new Date();
    entity.setCreatedDate(now);
    entity.setUpdatedDate(now);

    pgClient.withTrans(conn -> usersService.getUserByIdForUpdate(conn, entity.getId())
      .compose(userFromStorage -> {
        if (userFromStorage == null) {
          return succeededFuture(PutUsersByUserIdResponse.respond404WithTextPlain(entity.getId()));
        }

        return userTenantService.validateUserAcrossTenants(entity, userFromStorage, okapiHeaders, conn, vertxContext)
          .compose(aVoid -> usersService.updateUser(conn, entity)
            .onSuccess(user -> {
              if (StringUtils.equals(UserType.SHADOW.getTypeName(), user.getType())) {
                logger.info("Skip sending Update domain event for shadow user with id: {}", user.getId());
                return;
              }
              userEventPublisher(vertxContext, okapiHeaders).publishUpdated(entity.getId(), userFromStorage, user);
            })
            .compose(user -> userOutboxService.saveUserOutboxLogForUpdateUser(conn, user, userFromStorage, okapiHeaders))
            .map(isUserOutboxLogSaved -> PutUsersByUserIdResponse.respond204())
            .map(Response.class::cast));
      })
    ).onComplete(reply -> {
      if (reply.cause() != null) {
        handleUpdateUserFailures(entity, asyncResultHandler, reply);
        return;
      }
      userOutboxService.processOutboxEventLogs(vertxContext.owner(), okapiHeaders);
      asyncResultHandler.handle(reply);
    });
  }

  private void handleUpdateUserFailures(User user, Handler<AsyncResult<Response>> asyncResultHandler, AsyncResult<Response> reply) {
    String errorMessage = reply.cause().getMessage();
    if (isDuplicateUsernameError(errorMessage)) {
      logger.warn("User with this username {} already exists", user.getUsername());
      asyncResultHandler.handle(
        succeededFuture(PutUsersByUserIdResponse
          .respond400WithTextPlain(DUPLICATE_USERNAME_ERROR)));
      return;
    }

    if (isDuplicateBarcodeError(errorMessage)) {
      logger.warn("This barcode {} has already been taken", user.getBarcode());
      asyncResultHandler.handle(
        succeededFuture(PutUsersByUserIdResponse
          .respond400WithTextPlain(DUPLICATE_BARCODE_ERROR)));
      return;
    }

    if (isInvalidUserTypeError(errorMessage)) {
      logger.warn("An invalid user type {} has been populated to a user with id {}", user.getType(), user.getId());
      asyncResultHandler.handle(
        succeededFuture(PutUsersByUserIdResponse
          .respond400WithTextPlain(String.format(INVALID_USER_TYPE_ERROR,
              Arrays.stream(UserType.values()).map(UserType::getTypeName).toList()))));
      return;
    }

    if (isInvalidUsernameError(errorMessage)) {
      logger.warn("The user with the ID {} must have a username since consortium mode is enabled", user.getId());
      asyncResultHandler.handle(
        succeededFuture(PutUsersByUserIdResponse
          .respond400WithTextPlain(String.format(INVALID_USERNAME_ERROR, user.getId()))));
      return;
    }

    if (reply.cause() instanceof PgException pgException) {
      String errorMsg = pgException.getDetail();
      logger.error("DB error thrown with message: {}", errorMsg);
      asyncResultHandler.handle(
        succeededFuture(PutUsersByUserIdResponse
          .respond400WithTextPlain(errorMsg)));
      return;
    }

    logger.error(errorMessage);
    asyncResultHandler.handle(
      succeededFuture(PutUsersByUserIdResponse
        .respond400WithTextPlain(errorMessage))
    );
  }

  private String validateDateOfBirth(User user) {
    // manual test needed because RAML's minimum constraint supports numbers only, not dates

    if (user.getPersonal() == null) {
      return null;
    }
    var dateOfBirth = user.getPersonal().getDateOfBirth();
    if (dateOfBirth == null || dateOfBirth.compareTo(year1) >= 0) {
      return null;
    }
    return "dateOfBirth must be at least 0001-01-01";
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
      .toList();

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

  private Config mapResultSetToConfig(RowSet<Row> resultSet) {
    Config config = new Config();
    for (Row row : resultSet) {
      config
        .withId(row.getValue(ID).toString())
        .withConfigName(row.getValue(CONFIG_NAME).toString())
        .withEnabledObjectStorage(row.getJsonObject(JSONB).getBoolean(ENABLED_OBJECT_STORAGE))
        .withEnabled(row.getJsonObject(JSONB).getBoolean(ENABLED))
        .withEncryptionKey(row.getJsonObject(JSONB).getString(ENCRYPTION_KEY))
        .withMaxFileSize(row.getJsonObject(JSONB).getDouble(MAX_FILE_SIZE));
    }
    return config;
  }

  private static String createDeleteQuery(CQLWrapper wrapper, Map<String, String> okapiHeaders) {
    return String.format(DELETE_USERS_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_USERS + " " + wrapper.getWhereClause() + " " + RETURNING_USERS_ID_SQL);
  }

  private static String createDeleteQuery(Map<String, String> okapiHeaders) {
    return String.format(DELETE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }

}
