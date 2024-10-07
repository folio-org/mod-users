package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PgUtil;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Map;

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
    var criterion = new Criterion(new Criteria().addField("id").setJSONB(false).setOperation("=").setVal(id));
    PgUtil.postgresClient(vertxContext, okapiHeaders)
        .withTrans(conn -> conn.get(STAGING_USERS_TABLE, StagingUser.class, criterion)
        .compose(stagingUser -> {
          if (!stagingUser.getResults().isEmpty()) {
            log.info("Staging User :: {}", stagingUser.getResults().get(0));
          } else {
            log.info("Staging User is not found");
          }
          return Future.succeededFuture(StagingUsers.PutStagingUsersMergeByIdResponse.respond200())
            .map(Response.class :: cast);
          }))
      .onComplete(asyncResultHandler);
  }


}
