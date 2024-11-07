package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.service.UserGroupService;

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

    new UserGroupService(vertxContext, okapiHeaders).findByQuery(query, offset, limit)
      .onComplete(asyncResultHandler);
  }

  @Validate
  @Override
  public void postGroups(Usergroup entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    new UserGroupService(vertxContext, okapiHeaders).create(entity)
      .onComplete(asyncResultHandler);
  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    new UserGroupService(vertxContext, okapiHeaders).findById(groupId)
      .onComplete(asyncResultHandler);
  }

  @Validate
  @Override
  public void deleteGroupsByGroupId(String groupId, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    new UserGroupService(vertxContext, okapiHeaders).deleteById(groupId)
      .onComplete(asyncResultHandler);
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, Usergroup entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    new UserGroupService(vertxContext, okapiHeaders).updateById(entity, groupId)
      .onComplete(asyncResultHandler);
  }
}
