package org.folio.rest.impl;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.RestVerticle;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.GroupsResource;
import org.folio.rest.jaxrs.resource.UsersResource.GetUsersResponse;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.OutStream;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.ValidationHelper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.FieldException;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements GroupsResource {

  public static final String       GROUP_TABLE           = "groups";
  public static final String       GROUP_USER_JOIN_TABLE = "groups_users";
  public static final String       ID_FIELD_NAME         = "id";

  private static final String       LOCATION_PREFIX       = "/groups/";
  private static final Logger       log                   = LoggerFactory.getLogger(UserGroupAPI.class);
  private final Messages            messages              = Messages.getInstance();


  public UserGroupAPI(Vertx vertx, String tenantId) {
    PostgresClient.getInstance(vertx, tenantId).setIdField(ID_FIELD_NAME);
  }

  @Validate
  @Override
  public void getGroups(String query, int offset, int limit,
      String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {

    /**
    * http://host:port/groups
    */
    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... getGroups");
        String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
        CQLWrapper cql = getCQL(query,limit, offset);

        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(GROUP_TABLE, Usergroup.class,
          new String[]{"*"}, cql, true, true,
            reply -> {
              try {
                if(reply.succeeded()){
                  Usergroups groups = new Usergroups();
                  @SuppressWarnings("unchecked")
                  List<Usergroup> groupList = (List<Usergroup>) reply.result()[0];
                  groups.setUsergroups(groupList);
                  groups.setTotalRecords((Integer)reply.result()[1]);
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse.withJsonOK(
                    groups)));
                }
                else{
                  log.error(reply.cause().getMessage(), reply.cause());
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
                    .withPlainBadRequest(reply.cause().getMessage())));
                }
              } catch (Exception e) {
                log.error(e.getMessage(), e);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
                  .withPlainInternalServerError(messages.getMessage(
                    lang, MessageConsts.InternalServerError))));
              }
            });
      }
      catch(FieldException fe){
        log.error(fe.getLocalizedMessage(), fe);
        asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withPlainBadRequest(
                "CQL Parsing Error for '" + query + "': " + fe.getLocalizedMessage())));
      }
      catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });

  }

  @Validate
  @Override
  public void postGroups(String lang, Usergroup entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {

    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... postGroups");
        String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
        PostgresClient.getInstance(vertxContext.owner(), tenantId).save(
          GROUP_TABLE,
          entity,
          reply -> {
            try {
              if(reply.succeeded()){
                Object ret = reply.result();
                entity.setId((String) ret);
                OutStream stream = new OutStream();
                stream.setData(entity);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse.withJsonCreated(
                  LOCATION_PREFIX + ret, stream)));
              }
              else{
                log.error(reply.cause().getMessage(), reply.cause());
                if(isDuplicate(reply.cause().getMessage())){
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                    .withJsonUnprocessableEntity(ValidationHelper.createValidationErrorMessage(
                      "group", entity.getGroup(), "Group exists"))));
                }
                else{
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                    .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
                }
              }
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
            }
          });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });

  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {

    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... getGroupsByGroupId");
        String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );

        Criterion c = new Criterion(
          new Criteria().addField(ID_FIELD_NAME).setJSONB(false).setOperation("=").setValue("'"+groupId+"'"));

        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(GROUP_TABLE, Usergroup.class, c, true,
            reply -> {
              try {
                if(reply.succeeded()){
                  @SuppressWarnings("unchecked")
                  List<Usergroup> userGroup = (List<Usergroup>) reply.result()[0];
                  if(userGroup.isEmpty()){
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .withPlainNotFound(groupId)));
                  }
                  else{
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .withJsonOK(userGroup.get(0))));
                  }
                }
                else{
                  log.error(reply.cause().getMessage(), reply.cause());
                  if(isInvalidUUID(reply.cause().getMessage())){
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .withPlainNotFound(groupId)));
                  }
                  else{
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
                  }
                }
              } catch (Exception e) {
                log.error(e.getMessage(), e);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                  .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
        });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  @Validate
  @Override
  public void deleteGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {

    vertxContext.runOnContext(v -> {
      System.out.println("sending... deleteGroupsByGroupId");
      String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
      try {
        User u = new User();
        u.setPatronGroup(groupId);
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(UsersAPI.TABLE_NAME_USER, u, true, false,
          replyHandler -> {
          if(replyHandler.succeeded()){
            List<User> userList = (List<User>) replyHandler.result()[0];
            if(userList.size() > 0){
              log.error("Can not delete group, "+ groupId + ". " + userList.size()  + " users associated with it");
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                .withPlainBadRequest("Can not delete group, " + userList.size()  + " users associated with it")));
              return;
            }
            else{
              log.info("Deleting empty group, "+ groupId);
            }
            try {
              PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(GROUP_TABLE, groupId,
                reply -> {
                  try {
                    if(reply.succeeded()){
                      if(reply.result().getUpdated() == 1){
                        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                          .withNoContent()));
                      }
                      else{
                        log.error(messages.getMessage(lang, MessageConsts.DeletedCountError, 1, reply.result().getUpdated()));
                        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                          .withPlainNotFound(messages.getMessage(lang, MessageConsts.DeletedCountError,1 , reply.result().getUpdated()))));
                      }
                    }
                    else{
                      log.error(reply.cause().getMessage(), reply.cause());
                      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                        .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
                    }
                  } catch (Exception e) {
                    log.error(e.getMessage(), e);
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                      .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
                  }
                });
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
            }
          }
          else{
            log.error(replyHandler.cause().getMessage(), replyHandler.cause());
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
              .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
        });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, String lang, Usergroup entity,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) throws Exception {

    vertxContext.runOnContext(v -> {
      System.out.println("sending... putGroupsByGroupId");
      String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
      try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
          GROUP_TABLE, entity, groupId,
          reply -> {
            try {
              if(reply.succeeded()){
                if(reply.result().getUpdated() == 0){
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                    .withPlainNotFound(messages.getMessage(lang, MessageConsts.NoRecordsUpdated))));
                }
                else{
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                    .withNoContent()));
                }
              }
              else{
                log.error(reply.cause().getMessage());
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                  .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
            }
          });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  private CQLWrapper getCQL(String table, String query, int limit, int offset) throws FieldException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(table+".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  private CQLWrapper getCQL(String query, int limit, int offset) throws FieldException {
    return getCQL(GROUP_TABLE, query, limit, offset);
  }

  private boolean isDuplicate(String errorMessage){
    if(errorMessage != null && errorMessage.contains("duplicate key value violates unique constraint")){
      return true;
    }
    return false;
  }

  private boolean isInvalidUUID(String errorMessage){
    if(errorMessage != null && errorMessage.contains("invalid input syntax for uuid")){
      return true;
    }
    else{
      return false;
    }
  }
}
