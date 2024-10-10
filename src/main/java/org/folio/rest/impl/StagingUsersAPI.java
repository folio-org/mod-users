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

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static io.vertx.core.Future.succeededFuture;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {
  private static final Logger logger = LogManager.getLogger(StagingUsersAPI.class);
  public static final String STAGING_USERS_TABLE = "staging_users";

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  @Override
  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
    final Criterion criterion = new Criterion(
      new Criteria().addField("'contactInfo'").addField("'email'")
        .setOperation("=").setVal(entity.getContactInfo().getEmail()).setJSONB(true));
    postgresClient.withTrans(conn -> {
      return conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion, true)
        .compose(stagingUserResults -> {
          logger.info("record found by email success1: {} ", stagingUserResults.getResultInfo().getTotalRecords());
          logger.info("record found by email success2: {} ", stagingUserResults.getResults().size());
          List<StagingUser> stagingUsersByEmail = stagingUserResults.getResults();
          StagingUser existingStagingUser = Optional.ofNullable(stagingUsersByEmail)
            .filter(l->!l.isEmpty())
            .map(l->l.get(0)).orElse(null);
          String entityId = updateMetaDataAndEntityId(okapiHeaders, existingStagingUser);
          return conn.upsert(STAGING_USERS_TABLE, entityId, entity, true)
            .compose(id-> conn.getById(STAGING_USERS_TABLE, id, StagingUser.class));
        });
    }).onFailure(handler ->
      asyncResultHandler.handle(
        succeededFuture(PostStagingUsersResponse.respond500WithTextPlain(handler.getMessage()))
      )
    ).onSuccess(stagingUser ->
      asyncResultHandler.handle(
        succeededFuture(StagingUsers.PostStagingUsersResponse.respond201WithApplicationJson(stagingUser, PostStagingUsersResponse.headersFor201()))
      ));
  }

  private String updateMetaDataAndEntityId(Map<String, String> okapiHeaders, StagingUser existingStagingUser) {
    if(existingStagingUser != null) {
      String entityId = existingStagingUser.getId();
      String userId = okapiHeaders.get(RestVerticle.OKAPI_USERID_HEADER);
      Metadata metadata = existingStagingUser.getMetadata();
      metadata.setUpdatedDate(new Date());
      metadata.setUpdatedByUserId(userId);
      return entityId;
    }
    return null;
  }


}
