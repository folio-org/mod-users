package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.PgUtil;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Map;

@Path("staging-users")
public class StagingUsersAPI implements StagingUsers {

  public static final String STAGING_USERS_TABLE = "staging_users";

  @Override
  public void getStagingUsers(String query, String orderBy, StagingUsersGetOrder order, String totalRecords, int offset,
                              int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PgUtil.get(STAGING_USERS_TABLE, StagingUser.class, StagingUserdataCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, StagingUsers.GetStagingUsersResponse.class, asyncResultHandler);
  }

  @Override
  public void postStagingUsers(StagingUser entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    throw new UnsupportedOperationException("postStagingUsers api is not supported currently");
  }


}
