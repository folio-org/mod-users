package org.folio.rest.impl;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.utils.PostgresClientUtil;
import org.folio.rest.utils.ValidationHelper;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.UUID;
import org.folio.rest.persist.PgUtil;

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

    String id = entity.getId();
    if (id == null) {
      id = UUID.randomUUID().toString();
      entity.setId(id);
    }
    PostgresClientUtil.getInstance(vertxContext, okapiHeaders).save(
      GROUP_TABLE, id, entity, reply -> {
        if (reply.succeeded()) {
          String ret = reply.result();
          entity.setId(ret);
          asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
            PostGroupsResponse.respond201WithApplicationJson(entity,
              PostGroupsResponse.headersFor201().withLocation(LOCATION_PREFIX + ret))));
        } else {
          log.error(reply.cause().getMessage(), reply.cause());
          if (isDuplicate(reply.cause().getMessage())) {
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
              PostGroupsResponse.respond422WithApplicationJson(
                ValidationHelper.createValidationErrorMessage(
                  "group", entity.getGroup(), "Group exists"))));
          } else {
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
              PostGroupsResponse.respond500WithTextPlain(
                messages.getMessage(lang,
                  MessageConsts.InternalServerError))));
          }
        }
      });
  }

  @Validate
  @Override
  public void getGroupsByGroupId(String groupId, String lang, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // cannot use PgUtil.getById .. returns 500 where the below returns 404
    vertxContext.runOnContext(v -> {
      try {
        System.out.println("sending... getGroupsByGroupId");

       Criterion c = new Criterion(
          new Criteria().addField(ID_FIELD_NAME).setJSONB(false).setOperation("=").setValue("'"+groupId+"'"));

        PostgresClientUtil.getInstance(vertxContext, okapiHeaders).get(GROUP_TABLE, Usergroup.class, c, false,
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

    Criterion criterion = new Criterion(
      new Criteria()
        .addField(ID_FIELD_NAME)
        .setJSONB(false)
        .setOperation("=")
        .setValue("'" + groupId + "'"));
    PostgresClient postgresClient = PostgresClientUtil.getInstance(vertxContext, okapiHeaders);
    postgresClient.get(GROUP_TABLE, Usergroup.class, criterion, true, getReply -> {
      if (getReply.failed()) {
        log.error(getReply.cause().getMessage(), getReply.cause());
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
          .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
      } else {
        List<Usergroup> userGroup = getReply.result().getResults();
        if (userGroup.isEmpty()) {
          asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
            .respond404WithTextPlain(groupId)));
          return;
        }
        User u = new User();
        u.setPatronGroup(groupId);
        postgresClient.get(UsersAPI.TABLE_NAME_USERS, u, true, false, replyHandler -> {
          if (replyHandler.succeeded()) {
            List<User> userList = replyHandler.result().getResults();
            if (userList.size() > 0) {
              log.error("Can not delete group, " + groupId + ". " + userList.size() + " users associated with it");
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
                .respond400WithTextPlain("Can not delete group, " + userList.size() + " users associated with it")));
              return;
            } else {
              log.info("Deleting empty group, " + groupId);
            }
            PgUtil.deleteById(GROUP_TABLE, groupId, okapiHeaders, vertxContext,
              DeleteGroupsByGroupIdResponse.class, asyncResultHandler);
          } else {
            log.error(replyHandler.cause().getMessage(), replyHandler.cause());
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(DeleteGroupsByGroupIdResponse
              .respond500WithTextPlain(messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
        });
      }
    });
  }

  @Validate
  @Override
  public void putGroupsByGroupId(String groupId, String lang, Usergroup entity,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    // PgUtil.put returns differnet status code , so can't be used here.
    // probably 500 where it should have been 400/404
    PostgresClientUtil.getInstance(vertxContext, okapiHeaders).update(
      GROUP_TABLE, entity, groupId,
      reply -> {
        try {
          if (reply.succeeded()) {
            if (reply.result().getUpdated() == 0) {
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                .respond404WithTextPlain(messages.getMessage(lang, MessageConsts.NoRecordsUpdated))));
            } else {
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PutGroupsByGroupIdResponse
                .respond204()));
            }
          } else {
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
  }

  private boolean isDuplicate(String errorMessage) {
    return errorMessage != null && errorMessage.contains("duplicate key value violates unique constraint");
  }

  private boolean isInvalidUUID(String errorMessage) {
    return errorMessage != null && errorMessage.contains("uuid");
  }
}
