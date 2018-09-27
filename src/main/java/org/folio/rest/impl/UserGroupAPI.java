package org.folio.rest.impl;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.RestVerticle;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.jaxrs.resource.Users.GetUsersResponse;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.ResourceUtils;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.ValidationHelper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSONException;
import org.z3950.zing.cql.cql2pgjson.FieldException;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.UUID;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements Groups {

  public static final String       GROUP_TABLE           = "groups";
  public static final String       GROUP_USER_JOIN_TABLE = "groups_users";
  public static final String       ID_FIELD_NAME         = "id";

  private static final String       LOCATION_PREFIX       = "/groups/";
  private static final Logger       log                   = LoggerFactory.getLogger(UserGroupAPI.class);
  private final Messages            messages              = Messages.getInstance();

  private static final String GROUP_SCHEMA_PATH = UsersAPI.RAML_PATH + "/schemas/mod-users/usergroup.json";
  static final String GROUP_SCHEMA = schema(GROUP_SCHEMA_PATH);

  // TODO: replace by ResourceUtils.resource2String: https://issues.folio.org/browse/RMB-258
  private static String schema(String path) {
    try {
      return ResourceUtils.resource2String(path);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  public UserGroupAPI(Vertx vertx, String tenantId) {
    PostgresClient.getInstance(vertx, tenantId).setIdField(ID_FIELD_NAME);
  }

  @Validate
  @Override
  public void getGroups(String query, int offset, int limit,
      String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

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
                  List<Usergroup> groupList = reply.result().getResults();
                  groups.setUsergroups(groupList);
                  groups.setTotalRecords(reply.result().getResultInfo().getTotalRecords());
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
                    .respond200WithApplicationJson(groups)));
                }
                else{
                  log.error(reply.cause().getMessage(), reply.cause());
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
                    .respond400WithTextPlain(reply.cause().getMessage())));
                }
              } catch (Exception e) {
                log.error(e.getMessage(), e);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
                  .respond500WithTextPlain(messages.getMessage(
                    lang, MessageConsts.InternalServerError))));
              }
            });
      }
      catch(FieldException fe){
        log.error(fe.getLocalizedMessage(), fe);
        asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.respond400WithTextPlain(
                "CQL Parsing Error for '" + query + "': " + fe.getLocalizedMessage())));
      }
      catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsResponse
          .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });

  }

  @Validate
  @Override
  public void postGroups(String lang, Usergroup entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {

    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... postGroups");
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(
                RestVerticle.OKAPI_HEADER_TENANT));
        String id = entity.getId();
        if(id == null) {
          id = UUID.randomUUID().toString();
          entity.setId(id);
        }
        PostgresClient.getInstance(vertxContext.owner(), tenantId).save(
          GROUP_TABLE, id, entity, reply -> {
            try {
              if(reply.succeeded()){
                String ret = reply.result();
                entity.setId(ret);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                  PostGroupsResponse.respond201WithApplicationJson(entity,
                    PostGroupsResponse.headersFor201().withLocation(LOCATION_PREFIX + ret))));
              }
              else{
                log.error(reply.cause().getMessage(), reply.cause());
                if(isDuplicate(reply.cause().getMessage())){
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                          PostGroupsResponse.respond422WithApplicationJson(
                          ValidationHelper.createValidationErrorMessage(
                          "group", entity.getGroup(), "Group exists"))));
                }
                else{
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                          PostGroupsResponse.respond500WithTextPlain(
                          messages.getMessage(lang,
                          MessageConsts.InternalServerError))));
                }
              }
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                      PostGroupsResponse.respond500WithTextPlain(
                      messages.getMessage(lang, MessageConsts.InternalServerError))));
            }
          });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                PostGroupsResponse.respond500WithTextPlain(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });

  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

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
                  List<Usergroup> userGroup = reply.result().getResults();
                  if(userGroup.isEmpty()){
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .respond404WithTextPlain(groupId)));
                  }
                  else{
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .respond200WithApplicationJson(userGroup.get(0))));
                  }
                }
                else{
                  log.error(reply.cause().getMessage(), reply.cause());
                  if(isInvalidUUID(reply.cause().getMessage())){
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .respond404WithTextPlain(groupId)));
                  }
                  else{
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                      .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
                  }
                }
              } catch (Exception e) {
                log.error(e.getMessage(), e);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                  .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
        });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
          .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  @Validate
  @Override
  public void deleteGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    vertxContext.runOnContext(v -> {
      System.out.println("sending... deleteGroupsByGroupId");
      String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
      try {
        Criterion criterion = new Criterion(
          new Criteria()
            .addField(ID_FIELD_NAME)
            .setJSONB(false)
            .setOperation("=")
            .setValue("'"+ groupId +"'"));
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                GROUP_TABLE, Usergroup.class, criterion, true, getReply -> {
          try {
            if(getReply.failed()) {
               log.error(getReply.cause().getMessage(), getReply.cause());
               asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                 .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
            } else {
              List<Usergroup> userGroup = getReply.result().getResults();
              if(userGroup.isEmpty()) {
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                        .respond404WithTextPlain(groupId)));
                return;
              }
              User u = new User();
              u.setPatronGroup(groupId);
              PostgresClient.getInstance(vertxContext.owner(), tenantId).get(UsersAPI.TABLE_NAME_USERS, u, true, false,
                replyHandler -> {
                if(replyHandler.succeeded()) {
                  List<User> userList = replyHandler.result().getResults();
                  if(userList.size() > 0){
                    log.error("Can not delete group, "+ groupId + ". " + userList.size()  + " users associated with it");
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                      .respond400WithTextPlain("Can not delete group, " + userList.size()  + " users associated with it")));
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
                                .respond204()));
                            }
                            else{
                              log.error(messages.getMessage(lang, MessageConsts.DeletedCountError, 1, reply.result().getUpdated()));
                              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                                .respond404WithTextPlain(messages.getMessage(lang, MessageConsts.DeletedCountError,1 , reply.result().getUpdated()))));
                            }
                          }
                          else{
                            log.error(reply.cause().getMessage(), reply.cause());
                            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                              .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
                          }
                        } catch (Exception e) {
                          log.error(e.getMessage(), e);
                          asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                            .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
                        }
                      });
                  } catch (Exception e) {
                    log.error(e.getMessage(), e);
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                      .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
                  }
                }
                else{
                  log.error(replyHandler.cause().getMessage(), replyHandler.cause());
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                    .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
                }
              });
            }
          } catch(Exception e) {
            log.error(e.getMessage(), e);
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
              .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
        });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
          .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, String lang, Usergroup entity,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

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
                    .respond404WithTextPlain(messages.getMessage(lang, MessageConsts.NoRecordsUpdated))));
                }
                else{
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                    .respond204()));
                }
              }
              else{
                log.error(reply.cause().getMessage());
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                  .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
            }
          });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
          .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });
  }

  private CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException, IOException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(GROUP_TABLE + ".jsonb", GROUP_SCHEMA);
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
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
