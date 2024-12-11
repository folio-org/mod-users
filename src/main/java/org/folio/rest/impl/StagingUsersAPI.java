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
import java.util.concurrent.atomic.AtomicReference;

import static io.vertx.core.Future.succeededFuture;

import static org.folio.service.impl.StagingUserService.STAGING_USER_NOT_FOUND;
import static org.folio.service.impl.StagingUserService.USER_NOT_FOUND;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {
  public static final String STAGING_USERS_TABLE = "staging_users";
  private static final Logger logger = LogManager.getLogger(StagingUsersAPI.class);

  private static void updateMetaInfo(Map<String, String> okapiHeaders, StagingUser existingStagingUser) {
    // updating metadata
    Metadata metadata = existingStagingUser.getMetadata();
    metadata.setUpdatedDate(new Date());
    metadata.setUpdatedByUserId(MetadataUtil.createMetadata(okapiHeaders).getUpdatedByUserId());
    existingStagingUser.setMetadata(metadata);
  }

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("getStagingUsers:: query: {}, orderBy: {}, order: {}, totalRecords: {}, offset: {}, limit: {}",
      query, orderBy, order, totalRecords, offset, limit);
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders,
                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("postStagingUsers:: request body: {}", entity);

    try {
      PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

      postgresClient.withTrans(conn -> prepareAndSaveNewStagingUser(conn, entity))
        .onFailure(cause -> handleFailurePost(cause, asyncResultHandler))
        .onSuccess(stagingUser -> handleSuccessPost(stagingUser, asyncResultHandler));

    } catch (Exception e) {
      asyncResultHandler.handle(
        succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(e.getLocalizedMessage()))
      );
    }
  }

  private Future<StagingUser> prepareAndSaveNewStagingUser(Conn conn, StagingUser entity) {
    prepareNewStagingUser(entity);
    return conn.save(STAGING_USERS_TABLE, entity.getId(), entity)
      .compose(id -> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
  }


  private Criterion buildCriterionForEmail(StagingUser entity) {
    return new Criterion(
      new Criteria().addField("'contactInfo'").addField("'email'")
        .setOperation("=").setVal(entity.getContactInfo().getEmail()).setJSONB(true)
    );
  }

  private Future<StagingUser> processStagingUser(Conn conn, StagingUser entity, Criterion criterion,
                                                 Map<String, String> okapiHeaders, AtomicReference<Boolean> isUpdated) {

    return conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion, true)
      .compose(stagingUserResults -> {
        List<StagingUser> stagingUsersByEmail = stagingUserResults.getResults();
        String entityId = processExistingUserIfPresent(stagingUsersByEmail, entity, okapiHeaders, isUpdated);

        return conn.upsert(STAGING_USERS_TABLE, entityId, entity, true)
          .compose(id -> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
      });
  }

  private String processExistingUserIfPresent(List<StagingUser> stagingUsersByEmail, StagingUser entity,
                                              Map<String, String> okapiHeaders, AtomicReference<Boolean> isUpdated) {
    if (stagingUsersByEmail != null && !stagingUsersByEmail.isEmpty()) {
      StagingUser existingStagingUser = stagingUsersByEmail.get(0);
      String entityId = existingStagingUser.getId();

      logger.info("Processing existing staging user with ID: {}", entityId);
      BeanUtilsExtended.copyPropertiesNotNull(existingStagingUser, entity);
      updateMetaInfo(okapiHeaders, existingStagingUser);

      isUpdated.set(Boolean.TRUE);
      return entityId;
    }
    return null;
  }

  private void prepareNewStagingUser(StagingUser entity) {
    logger.info("Creating new Staging-User");
    entity.setExternalSystemId(UUID.randomUUID().toString());
    entity.setStatus(entity.getStatus() != null ? entity.getStatus() : StagingUser.Status.TIER_1);
    entity.setIsEmailVerified(entity.getIsEmailVerified() != null ? entity.getIsEmailVerified() : Boolean.FALSE);
  }

  private void handleFailurePost(Throwable cause, Handler<AsyncResult<Response>> asyncResultHandler) {
    asyncResultHandler.handle(
      succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(cause.getMessage()))
    );
  }

  private void handleSuccessPost(StagingUser stagingUser,
                                 Handler<AsyncResult<Response>> asyncResultHandler) {
//    if (Boolean.TRUE.equals(isUpdated.get())) {
//      asyncResultHandler.handle(
//        succeededFuture(StagingUsers.PostStagingUsersResponse.respond200WithApplicationJson(stagingUser))
//      );
//    } else {
      asyncResultHandler.handle(
        succeededFuture(StagingUsers.PostStagingUsersResponse.respond201WithApplicationJson(stagingUser,
          PostStagingUsersResponse.headersFor201()))
      );
    //}
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
