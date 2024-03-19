package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.persist.PgUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements Groups {
  public static final String GROUP_TABLE = "groups";

  @Validate
  @Override
  public void getGroups(String query, String totalRecords, int offset, int limit,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(GROUP_TABLE, Usergroup.class, Usergroups.class, query, offset, limit, okapiHeaders,
      vertxContext, GetGroupsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void postGroups(Usergroup entity,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    PgUtil.post(GROUP_TABLE, entity, okapiHeaders, vertxContext,
      PostGroupsResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.getById(GROUP_TABLE, Usergroup.class, groupId, okapiHeaders, vertxContext,
        GetGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void deleteGroupsByGroupId(String groupId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.deleteById(GROUP_TABLE, groupId, okapiHeaders, vertxContext,
      DeleteGroupsByGroupIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, Usergroup entity,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    PgUtil.put(GROUP_TABLE, entity, groupId, okapiHeaders, vertxContext,
        PutGroupsByGroupIdResponse.class, asyncResultHandler);
  }
}
