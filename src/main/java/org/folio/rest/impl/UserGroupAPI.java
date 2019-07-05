package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.ValidationHelper;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements Groups {

  public static final String GROUP_TABLE = "groups";
  public static final String GROUP_USER_JOIN_TABLE = "groups_users";
  public static final String ID_FIELD_NAME = "id";

  private static final String LOCATION_PREFIX = "/groups/";
  private static final Logger log = LoggerFactory.getLogger(UserGroupAPI.class);
  private final Messages messages = Messages.getInstance();

  @Validate
  @Override
  public void getGroups(String query, int offset, int limit,
    String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(GROUP_TABLE, Usergroup.class, Usergroups.class, query, offset, limit, okapiHeaders,
      vertxContext, GetGroupsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void postGroups(String lang, Usergroup entity,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.post(GROUP_TABLE, entity, okapiHeaders, vertxContext, PostGroupsResponse.class, post -> {
      try {
        if (post.succeeded() && isDuplicate(post.result().getEntity().toString())) {
          asyncResultHandler.handle(Future.succeededFuture(
              PostGroupsResponse.respond422WithApplicationJson(
                ValidationHelper.createValidationErrorMessage(
                  "group", entity.getGroup(), "Group exists"))));
          return;
        }
        asyncResultHandler.handle(post);
      } catch (Exception e) {
        ValidationHelper.handleError(e, asyncResultHandler);
      }
    });
  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.getById(GROUP_TABLE, Usergroup.class, groupId, okapiHeaders, vertxContext,
        GetGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void deleteGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.deleteById(GROUP_TABLE, groupId, okapiHeaders, vertxContext,
        DeleteGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, String lang, Usergroup entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.put(GROUP_TABLE, entity, groupId, okapiHeaders, vertxContext,
        PutGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  private boolean isDuplicate(String errorMessage) {
    return errorMessage != null && errorMessage.contains("duplicate key value violates unique constraint");
  }
}
