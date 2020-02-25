package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.utils.ValidationHelper;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements Groups {

  public static final String GROUP_TABLE = "groups";
  public static final String GROUP_USER_JOIN_TABLE = "groups_users";
  private static final String PATRON_BLOCK_LIMITS_TABLE = "patron_block_limits";
  public static final String ID_FIELD_NAME = "id";
  public static final String PATRON_GROUP_ID_FIELD = "patronGroupId";

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
          asyncResultHandler.handle(succeededFuture(
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
      DeleteGroupsByGroupIdResponse.class, delete -> {
      try {
        if (delete.succeeded() && delete.result().getStatus() == HTTP_NO_CONTENT) {
          String query = PATRON_GROUP_ID_FIELD + "=" + groupId;
          CQL2PgJSON cql2pgJson = new CQL2PgJSON(PATRON_BLOCK_LIMITS_TABLE + ".jsonb");
          CQLWrapper cqlWrapper = new CQLWrapper(cql2pgJson, query);

          PgUtil.postgresClient(vertxContext, okapiHeaders)
            .delete(PATRON_BLOCK_LIMITS_TABLE, cqlWrapper, reply -> {
              DeleteGroupsByGroupIdResponse response = reply.failed()
                ? DeleteGroupsByGroupIdResponse.respond500WithTextPlain(reply.cause().getLocalizedMessage())
                : DeleteGroupsByGroupIdResponse.respond204();
              asyncResultHandler.handle(succeededFuture(response));
            });
          return;
        }
        asyncResultHandler.handle(delete);
      } catch (Exception e) {
        ValidationHelper.handleError(e, asyncResultHandler);
      }
    });
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, String lang, Usergroup entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    PgUtil.put(GROUP_TABLE, entity, groupId, okapiHeaders, vertxContext,
        PutGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  protected boolean isDuplicate(String errorMessage) {
    return errorMessage != null && errorMessage.contains("duplicate key value violates unique constraint");
  }
}
