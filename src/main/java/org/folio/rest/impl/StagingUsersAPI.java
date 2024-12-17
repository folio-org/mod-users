package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserMergeResponse;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.MetadataUtil;
import org.folio.rest.utils.BeanUtilsExtended;
import org.folio.service.impl.StagingUserService;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.service.impl.StagingUserService.STAGING_USER_NOT_FOUND;
import static org.folio.service.impl.StagingUserService.USER_NOT_FOUND;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {
  public static final String STAGING_USERS_TABLE = "staging_users";
  private static final Logger logger = LogManager.getLogger(StagingUsersAPI.class);

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("getStagingUsers:: query: {}, orderBy: {}, order: {}, totalRecords: {}, offset: {}, limit: {}",
      query, orderBy, order, totalRecords, offset, limit);
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  @Override
  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders,
                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("postStagingUsers:: request body: {}", entity);

    try {
      PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
      postgresClient.withTrans(conn -> prepareAndSaveNewStagingUser(conn, entity))
        .onFailure(cause -> handleFailure(cause, asyncResultHandler, true))
        .onSuccess(stagingUser -> handleSuccess(stagingUser, asyncResultHandler, true));
    } catch (Exception e) {
      handleFailure(e, asyncResultHandler, true);
    }
  }

  @Override
  public void putStagingUsersByExternalSystemId(String externalSystemId, StagingUser entity,
                                                Map<String, String> okapiHeaders,
                                                Handler<AsyncResult<Response>> asyncResultHandler,
                                                Context vertxContext) {
    logger.debug("putStagingUsersByExternalSystemId:: request body: {}", entity);

    try {
      PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
      final Criterion criterion = buildCriterionForExternalSystemId(externalSystemId);

      postgresClient.withTrans(conn -> processStagingUser(conn, entity, criterion, okapiHeaders))
        .onFailure(cause -> handleFailure(cause, asyncResultHandler, false))
        .onSuccess(stagingUser -> handleSuccess(stagingUser, asyncResultHandler, false));
    } catch (Exception e) {
      handleFailure(e, asyncResultHandler, true);
    }
  }

  private void handleFailure(Throwable cause, Handler<AsyncResult<Response>> asyncResultHandler, boolean isPost) {
    String errorMessage = cause.getMessage() != null ? cause.getMessage() : "An unexpected error occurred";
    if(isPost) {
      asyncResultHandler.handle(
        succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(errorMessage))
      );
    } else {
      if(errorMessage.equals("No matching user found for update.")) {
        asyncResultHandler.handle(
          succeededFuture(PutStagingUsersByExternalSystemIdResponse.respond404WithTextPlain(errorMessage)));
      } else {
        asyncResultHandler.handle(
          succeededFuture(PutStagingUsersByExternalSystemIdResponse.respond500WithTextPlain(errorMessage))
        );
      }
    }
  }

  private void handleSuccess(StagingUser stagingUser, Handler<AsyncResult<Response>> asyncResultHandler, boolean isPost) {
    if (isPost) {
      asyncResultHandler.handle(
        succeededFuture(StagingUsers.PostStagingUsersResponse.respond201WithApplicationJson(
          stagingUser, PostStagingUsersResponse.headersFor201()))
      );
    } else {
      asyncResultHandler.handle(
      succeededFuture(StagingUsers.PutStagingUsersByExternalSystemIdResponse.respond200WithApplicationJson(stagingUser))
      );
    }
  }

  private Future<StagingUser> prepareAndSaveNewStagingUser(Conn conn, StagingUser entity) {
    prepareNewStagingUser(entity);
    return conn.save(STAGING_USERS_TABLE, entity.getId(), entity)
      .compose(id -> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
  }

  private Future<StagingUser> processStagingUser(Conn conn, StagingUser entity, Criterion criterion,
                                                 Map<String, String> okapiHeaders) {
    return conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion, true)
      .compose(stagingUserResults -> {
        List<StagingUser> stagingUsersByExternalSystemId = stagingUserResults.getResults();
        String entityId = processExistingUserIfPresent(stagingUsersByExternalSystemId, entity, okapiHeaders);
        if (entityId != null) {
          return conn.upsert(STAGING_USERS_TABLE, entityId, entity, true)
            .compose(id -> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
        } else {
          return Future.failedFuture("No matching user found for update.");
        }
      });
  }

  private Criterion buildCriterionForExternalSystemId(String externalSystemId) {
    return new Criterion(
      new Criteria().addField("'externalSystemId'")
        .setOperation("=").setVal(externalSystemId).setJSONB(true)
    );
  }

  private String processExistingUserIfPresent(List<StagingUser> stagingUsersByExternalSystemId, StagingUser entity,
                                              Map<String, String> okapiHeaders) {
    if (stagingUsersByExternalSystemId != null && !stagingUsersByExternalSystemId.isEmpty()) {
      StagingUser existingStagingUser = stagingUsersByExternalSystemId.get(0);
      String entityId = existingStagingUser.getId();

      logger.info("Processing existing staging user with ID: {}", entityId);
      BeanUtilsExtended.copyPropertiesNotNull(existingStagingUser, entity);
      updateMetaInfo(okapiHeaders, existingStagingUser);

      return entityId;
    }
    return null;
  }

  private static void updateMetaInfo(Map<String, String> okapiHeaders, StagingUser existingStagingUser) {
    Metadata metadata = existingStagingUser.getMetadata();
    metadata.setUpdatedDate(new Date());
    metadata.setUpdatedByUserId(MetadataUtil.createMetadata(okapiHeaders).getUpdatedByUserId());
    existingStagingUser.setMetadata(metadata);
  }

  private void prepareNewStagingUser(StagingUser entity) {
    logger.info("Creating new Staging-User");
    entity.setExternalSystemId(UUID.randomUUID().toString());
    entity.setStatus(entity.getStatus() != null ? entity.getStatus() : StagingUser.Status.TIER_1);
    entity.setIsEmailVerified(entity.getIsEmailVerified() != null ? entity.getIsEmailVerified() : Boolean.FALSE);
  }

  @Override
  public void putStagingUsersMergeOrCreateUserById(String id, String userId, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("putStagingUsersMergeOrCreateUserById:: stagingUserId {} and userId {}", id, userId);
    new StagingUserService(vertxContext, okapiHeaders)
      .mergeOrCreateUserFromStagingUser(id, userId)
      .onSuccess(user -> {
        logger.info("putStagingUsersMergeOrCreateUserById:: user response {}", user);
        asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeOrCreateUserByIdResponse
          .respond200WithApplicationJson(new StagingUserMergeResponse().withUserId(user.getId()))));
      })
      .onFailure(throwable -> {
        var errorMessage = throwable.getMessage();
        logger.error("putStagingUsersMergeById:: future failed with error {}", errorMessage);
        if (isUserNotFoundError(errorMessage, userId)) {
          asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeOrCreateUserByIdResponse.respond404WithTextPlain(errorMessage)));
          return;
        }
        if (isStagingUserNotFoundError(errorMessage, id)) {
          asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeOrCreateUserByIdResponse.respond404WithTextPlain(errorMessage)));
          return;
        }
        asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeOrCreateUserByIdResponse.respond500WithTextPlain(throwable.getMessage())));
      });
  }

  public boolean isUserNotFoundError(String errorMsg, String userId) {
    return String.format(USER_NOT_FOUND, userId).equals(errorMsg);
  }

  public boolean isStagingUserNotFoundError(String errorMsg, String stagingUserId) {
    return String.format(STAGING_USER_NOT_FOUND, stagingUserId).equals(errorMsg);
  }
}
