package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.RestVerticle;
import org.folio.rest.annotations.Validate;
import org.folio.rest.dao.Group2User;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;
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
import org.folio.rest.persist.helpers.JoinBy;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.OutStream;
import org.folio.rest.tools.utils.TenantTool;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.FieldException;

/**
 * @author shale
 *
 */
public class UserGroupAPI implements GroupsResource {

  public static final String       GROUP_TABLE           = "groups";
  public static final String       GROUP_USER_JOIN_TABLE = "groups_users";

  private static final String       LOCATION_PREFIX       = "/groups/";
  private static final Logger       log                   = LoggerFactory.getLogger(UserGroupAPI.class);
  private final Messages            messages              = Messages.getInstance();

  private String idFieldName                              = "_id";

  public UserGroupAPI(Vertx vertx, String tenantId) {
    long nano = System.nanoTime();
    System.out.println("set id to " +idFieldName );
    PostgresClient.getInstance(vertx, tenantId).setIdField(idFieldName);
    long nanoend = System.nanoTime();
    System.out.println("total in milli " + ((nanoend-nano)/1000000));
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
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                  .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
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
          new Criteria().addField(idFieldName).setJSONB(false).setOperation("=").setValue("'"+groupId+"'"));

        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(GROUP_TABLE, Usergroup.class, c, true,
            reply -> {
              try {
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

    Group2User g2u = new Group2User();
    g2u.setGroupId(groupId);

    vertxContext.runOnContext(v -> {
      System.out.println("sending... deleteGroupsByGroupId");
      String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
      try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(GROUP_USER_JOIN_TABLE, g2u, true, false, replyHandler -> {
          if(replyHandler.succeeded()){
            List<Group2User> groupList = (List<Group2User>) replyHandler.result()[0];
            if(groupList.size() > 0){
              log.error("Can not delete group, "+ groupId + ". " + groupList.size()  + " users associated with it");
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                .withPlainBadRequest("Can not delete group, " + groupList.size()  + " users associated with it")));
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
                    .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.NoRecordsUpdated))));
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
  @Validate
  @Override
  public void getGroupsByGroupIdUsers(String groupId, String query,
      int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {

    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... getGroupsByGroupIdUsers");
        String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
        CQLWrapper cql = getCQL(query,limit, offset);

        //create a join between the users table and its external (non jsonb) id to the group to user table which
        //only contains a jsonb column (no id) - where in the jsonb column there is a groupId, userId fields

        JoinBy jbFrom = new JoinBy(UsersAPI.TABLE_NAME_USER, "users", new Criteria().addField("'id'"), new String[]{"jsonb"});

/* TO USE a non jsonb field as a constraint for the join - if for example it is the id field and the id field is of
 * type uuid - use the forceCast to cast to a varchar so that it can be compared to a value in a jsonb field that is textual
 * JoinBy jbFrom = new JoinBy(UsersAPI.TABLE_NAME_USER, "users", new Criteria().addField("_id").setJSONB(false)
   .setForceCast("varchar"), new String[]{"jsonb"});*/

        //do not return columns from the join table
        JoinBy jbOn = new JoinBy(GROUP_USER_JOIN_TABLE, "user2groups", new Criteria().addField("'userId'") , new String[]{});

        PostgresClient.getInstance(vertxContext.owner(), tenantId).join(jbFrom, jbOn, "=", JoinBy.INNER_JOIN, User.class, cql,
            reply -> {
              try {
                if(reply.succeeded()){
                  List<User> users = (List<User>) ((Object [])reply.result())[0];
                 // List<User> users = (List<User>)[0];
                  UserdataCollection userCollection = new UserdataCollection();
                  userCollection.setUsers(users);
                  userCollection.setTotalRecords((Integer)((Object [])reply.result())[1]);
                  asyncResultHandler.handle(Future.succeededFuture(
                          GetUsersResponse.withJsonOK(userCollection)));
                }
                else{
                  log.error(reply.cause().getMessage(), reply.cause());
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                    .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError)
                      + reply.cause().getMessage())));
                }
              } catch (Exception e) {
                log.error(e.getMessage(), e);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
                  .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
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
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetGroupsByGroupIdResponse
          .withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
      }
    });

  }

  @Validate
  @Override
  public void deleteGroupsByGroupIdUsers(String groupId, String lang,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) throws Exception {

    Group2User c = new Group2User();
    c.setGroupId(groupId);

    vertxContext.runOnContext(v -> {
      System.out.println("sending... deleteGroupsByGroupIdUsers");
      String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
      try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(GROUP_USER_JOIN_TABLE, c,
          reply -> {
            try {
              if(reply.succeeded()){
                log.info("All users ("+ reply.result().getUpdated() + ") deleted from group " + groupId);
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                  .withNoContent()));
              }
              else{
                log.error(reply.cause().getMessage());
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
    });

  }

  @Validate
  @Override
  public void putGroupsByGroupIdUsersByUserId(String userId, String groupId,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) throws Exception {

    Group2User g2u = new Group2User();
    g2u.setGroupId(groupId);
    g2u.setUserId(userId);

    //unique composite index on both fields so that the same entry is not added more than once
    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... putGroupsByGroupIdUsersByUserId");
        String tenantId = TenantTool.calculateTenantId( okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT) );
        PostgresClient.getInstance(vertxContext.owner(), tenantId).save(
          GROUP_USER_JOIN_TABLE, g2u, false,
          reply -> {
            try {
              if(reply.succeeded()){
                asyncResultHandler.handle(
                  io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdUsersByUserIdResponse.withNoContent()));
              }
              else{
                log.error(reply.cause().getMessage(), reply.cause());
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                  .withPlainInternalServerError(messages.getMessage("en", MessageConsts.InternalServerError))));
              }
            } catch (Exception e) {
              log.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
                .withPlainInternalServerError(messages.getMessage("en", MessageConsts.InternalServerError))));
            }
          });
      } catch (Exception e) {
        log.error(e.getMessage(), e);
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostGroupsResponse
          .withPlainInternalServerError(messages.getMessage("en", MessageConsts.InternalServerError))));
      }
    });
  }

  private CQLWrapper getCQL(String query, int limit, int offset) throws FieldException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(GROUP_TABLE+".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }
}
