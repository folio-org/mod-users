package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.emptyList;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;
import static org.folio.support.UsersApiConstants.VIEW_NAME_USER_GROUPS_JOIN;

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
      new Criteria().addField("contactInfo.email")
        .setOperation("=").setVal(entity.getContactInfo().getEmail()).setJSONB(true));

    postgresClient.withTrans(conn -> {
      return conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion, true)
        .compose(stagingUserResults -> {
          logger.info("record found by email success1: {} ", stagingUserResults.getResultInfo().getTotalRecords());
          logger.info("record found by email success2: {} ", stagingUserResults.getResults().size());
          List<StagingUser> stagingUsersByEmail = stagingUserResults.getResults();
          String entityId = Optional.ofNullable(stagingUsersByEmail)
            .filter(l->!l.isEmpty())
            .map(l->l.get(0))
            .map(StagingUser::getId).orElse(null);
          return conn.upsert(STAGING_USERS_TABLE, entityId, entity, true)
            .compose(id-> Future.succeededFuture(entity));
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


}
