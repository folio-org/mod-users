package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.Metadata;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.utils.BeanUtils;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static io.vertx.core.Future.succeededFuture;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {
  public static final String STAGING_USERS_TABLE = "staging_users";
  private static final Logger logger = LogManager.getLogger(StagingUsersAPI.class);

  private static void updateMetaInfo(Map<String, String> okapiHeaders, StagingUser existingStagingUser) {
    // updating metadata
    String userId = okapiHeaders.get(RestVerticle.OKAPI_USERID_HEADER);
    Metadata metadata = existingStagingUser.getMetadata();
    metadata.setUpdatedDate(new Date());
    metadata.setUpdatedByUserId(userId);
    existingStagingUser.setMetadata(metadata);
  }

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  @Override
  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders,
                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
      final Criterion criterion = new Criterion(
        new Criteria().addField("'contactInfo'").addField("'email'")
          .setOperation("=").setVal(entity.getContactInfo().getEmail()).setJSONB(true));
      AtomicReference<Boolean> isUpdated = new AtomicReference<>(Boolean.FALSE);
      postgresClient.withTrans(conn -> conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion, true)
        .compose(stagingUserResults -> {
          logger.info("Record found by email: {} ", stagingUserResults.getResults().size());
          List<StagingUser> stagingUsersByEmail = stagingUserResults.getResults();

          String entityId = null;
          if (stagingUsersByEmail != null && !stagingUsersByEmail.isEmpty()) {
            logger.info("Updating existing Staging-User");
            StagingUser existingStagingUser = stagingUsersByEmail.get(0);
            entityId = existingStagingUser.getId();
            logger.debug("Processing existing staging user with ID: {}", existingStagingUser.getId());

            // Copy non-null properties
            BeanUtils.copyPropertiesNotNull(existingStagingUser, entity);
            entity.setIsEmailVerified(existingStagingUser.getIsEmailVerified());

            updateMetaInfo(okapiHeaders, existingStagingUser);
            isUpdated.set(Boolean.TRUE);
          } else {
            logger.info("Creating new Staging-User");
            entity.setStatus(StagingUser.Status.TIER_1);
            entity.setIsEmailVerified(Boolean.FALSE);
            isUpdated.set(Boolean.FALSE);
          }
          return conn.upsert(STAGING_USERS_TABLE, entityId, entity, true)
            .compose(id -> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
        })).onFailure(handler ->
        asyncResultHandler.handle(
          succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(handler.getMessage()))
        )
      ).onSuccess(stagingUser -> {
        if (Boolean.TRUE.equals(isUpdated.get())) {
          asyncResultHandler.handle(
            succeededFuture(StagingUsers.PostStagingUsersResponse.respond200WithApplicationJson(stagingUser))
          );
        } else {
          asyncResultHandler.handle(
            succeededFuture(StagingUsers.PostStagingUsersResponse.respond201WithApplicationJson(stagingUser,
              PostStagingUsersResponse.headersFor201()))
          );
        }
      });
    } catch (Exception e) {
      asyncResultHandler.handle(
        succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(e.getLocalizedMessage()))
      );
    }
  }
}
