package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.StagingUser;
import org.folio.rest.jaxrs.model.StagingUserdataCollection;
import org.folio.rest.jaxrs.model.StagingUsersGetOrder;
import org.folio.rest.jaxrs.resource.StagingUsers;
import org.folio.rest.persist.PgUtil;
import org.folio.service.impl.StagingUserService;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.service.impl.StagingUserService.STAGING_USER_NOT_FOUND;
import static org.folio.service.impl.StagingUserService.USER_NOT_FOUND;

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
      new StagingUserService(vertxContext, okapiHeaders)
        .mergeStagingUserWithUser(id, userId)
      .onSuccess(user -> {
        log.info("user response {}", user);
        asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeByIdResponse.respond200WithApplicationJson(user.getId())));
      })
      .onFailure(throwable -> {
        var errorMessage = throwable.getMessage();
        log.error("putStagingUsersMergeById:: future failed with error {}", errorMessage);
        if (isUserNotFoundError(errorMessage, userId)) {
          asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeByIdResponse.respond404WithTextPlain(errorMessage)));
          return;
        }
        if (isStagingUserNotFoundError(errorMessage, id)) {
          asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeByIdResponse.respond404WithTextPlain(errorMessage)));
          return;
        }
        asyncResultHandler.handle(succeededFuture(PutStagingUsersMergeByIdResponse.respond500WithTextPlain(throwable.getMessage())));
      })  ;
  }

  public boolean isUserNotFoundError(String errorMsg, String userId) {
    return String.format(USER_NOT_FOUND, userId).equals(errorMsg);
  }

  public boolean isStagingUserNotFoundError(String errorMsg, String stagingUserId) {
    return String.format(STAGING_USER_NOT_FOUND, stagingUserId).equals(errorMsg);
  }

}
