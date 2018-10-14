package org.folio.rest.impl;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.function.Function;

import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.facets.FacetField;
import org.folio.rest.persist.facets.FacetManager;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.ResourceUtils;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.ValidationHelper;
import org.z3950.zing.cql.CQLParseException;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSONException;
import org.z3950.zing.cql.cql2pgjson.FieldException;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.jaxrs.model.UsersGetOrder;


/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements Users {

  public static final String TABLE_NAME_USERS = "users";
  public static final String VIEW_NAME_USER_GROUPS_JOIN = "users_groups_view";

  private final Messages messages = Messages.getInstance();
  public static final String USER_ID_FIELD = "'id'";
  public static final String USER_NAME_FIELD = "'username'";
  private static final String OKAPI_HEADER_TENANT = "x-okapi-tenant";
  private final Logger logger = LoggerFactory.getLogger(UsersAPI.class);
  public static final String RAML_PATH = "ramls";
  private static final String USER_SCHEMA_PATH = RAML_PATH + "/userdata.json";
  private static final String USER_SCHEMA = ResourceUtils.resource2String(USER_SCHEMA_PATH);
  private static final LinkedHashMap<String,String> fieldsAndSchemas = getFieldsAndSchemas();

  private static LinkedHashMap<String,String> getFieldsAndSchemas() {
    LinkedHashMap<String,String> map = new LinkedHashMap<>();
    map.put(VIEW_NAME_USER_GROUPS_JOIN+".jsonb", USER_SCHEMA);
    map.put(VIEW_NAME_USER_GROUPS_JOIN+".group_jsonb", UserGroupAPI.GROUP_SCHEMA);
    return map;
  }

  public UsersAPI(Vertx vertx, String tenantId) {
    PostgresClient.getInstance(vertx, tenantId).setIdField("id");
  }

  /**
   * right now, just query the join view if a cql was passed in, otherwise work with the
   * master users table. this can be optimized in the future to check if there is really a need
   * to use the join view due to cross table cqling - like returning users sorted by group name
   * @param cql
   * @return
   */
  private String getTableName(String cql) {
    if(cql != null && cql.contains("patronGroup.")){
      return VIEW_NAME_USER_GROUPS_JOIN;
      //}
    }
    return TABLE_NAME_USERS;
  }

  /**
   * check for entries in the cql which reference the group table, indicating a join is needed
   * and update the cql accordingly - by replacing the patronGroup. prefix with g. which is what
   * the view refers to the groups table
   * @param cql
   * @return
   */
  private static String convertQuery(String cql){
    if(cql != null){
      return cql.replaceAll("(?i)patronGroup\\.", VIEW_NAME_USER_GROUPS_JOIN+".group_jsonb.");
    }
    return cql;
  }

  static CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException, IOException {
    if(query != null && query.contains("patronGroup.")) {
      query = convertQuery(query);
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(fieldsAndSchemas);
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    } else {
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(TABLE_NAME_USERS+".jsonb", USER_SCHEMA);
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    }
  }

  private void handle(String message, Throwable e, Handler<AsyncResult<Response>> asyncResultHandler, String lang,
      Function<String,Response> report400, Function<String,Response> report500) {
    logger.error(message, e);

    Throwable cause = e;
    do {
      if (cause instanceof CQLParseException || cause instanceof FieldException) {
        asyncResultHandler.handle(Future.succeededFuture(report400.apply(
            "CQL Parsing Error for '" + message + "': " + cause.getMessage())));
        return;
      }
      if (cause instanceof IllegalStateException) {
        asyncResultHandler.handle(Future.succeededFuture(report400.apply(
            "CQL Illegal State Error for '" + message + "': " + cause.getMessage())));
        return;
      }
      cause = cause.getCause();
    } while (cause != null);

    asyncResultHandler.handle(Future.succeededFuture(report500.apply(
                messages.getMessage(lang, MessageConsts.InternalServerError))));
  }

  @Validate
  @Override
  public void getUsers(String query, String orderBy,
      UsersGetOrder order, int offset, int limit, List<String> facets,
      String lang, Map <String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    logger.debug("Getting users");
    try {
      CQLWrapper cql = getCQL(query,limit,offset);

      List<FacetField> facetList = FacetManager.convertFacetStrings2FacetFields(facets, "jsonb");

      String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
      String tableName = getTableName(query);
      String[] fieldList = {"*"};
      logger.debug("Headers present are: " + okapiHeaders.keySet().toString());
      logger.debug("tenantId = " + tenantId);

      PostgresClient.getInstance(vertxContext.owner(), tenantId)
          .get(tableName, User.class, fieldList, cql, true, false, facetList, reply -> {
        try {
          if (reply.succeeded()) {
            UserdataCollection userCollection = new UserdataCollection();
            List<User> users = reply.result().getResults();
            userCollection.setUsers(users);
            userCollection.setTotalRecords(reply.result().getResultInfo().getTotalRecords());
            userCollection.setResultInfo(reply.result().getResultInfo());
            asyncResultHandler.handle(Future.succeededFuture(
                GetUsersResponse.respond200WithApplicationJson(userCollection)));
          } else {
            handle(query, reply.cause(), asyncResultHandler, lang,
                GetUsersResponse::respond400WithTextPlain,
                GetUsersResponse::respond500WithTextPlain);
          }
        } catch (Exception e) {
          handle(query, e, asyncResultHandler, lang,
              GetUsersResponse::respond400WithTextPlain,
              GetUsersResponse::respond500WithTextPlain);
        }
      });
    } catch(Exception e) {
      handle(query, e, asyncResultHandler, lang,
          GetUsersResponse::respond400WithTextPlain,
          GetUsersResponse::respond500WithTextPlain);
    }
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    try {
      vertxContext.runOnContext( v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        String tableName = getTableName(null);
        if(checkForDuplicateAddressTypes(entity)) {
          asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.respond400WithTextPlain(
                      "Users are limited to one address per addresstype")));
          return;
        }
        try {
          Criteria idCrit = new Criteria(USER_SCHEMA_PATH);
          idCrit.addField(USER_ID_FIELD);
          idCrit.setOperation("=");
          idCrit.setValue(entity.getId());
          Criteria nameCrit = new Criteria(USER_SCHEMA_PATH);
          nameCrit.addField(USER_NAME_FIELD);
          nameCrit.setOperation("=");
          nameCrit.setValue(entity.getUsername());
          Criterion crit = new Criterion();
          crit.addCriterion(idCrit, "OR", nameCrit);

          checkAllAddressTypesValid(entity, vertxContext, tenantId).setHandler(
                  checkRes -> {
            if(checkRes.failed()) {
              logger.error(checkRes.cause().getLocalizedMessage(), checkRes.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.respond500WithTextPlain(
                  messages.getMessage(lang, MessageConsts.InternalServerError))));
            } else if(checkRes.result() == false) {
              asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.respond400WithTextPlain(
                        "You cannot add addresses with non-existant address types")));
            } else {
              try {
                PostgresClient.getInstance(vertxContext.owner(),
                        TenantTool.calculateTenantId(tenantId)).get(tableName,
                        User.class, crit, true, getReply -> {
                    logger.debug("Attempting to get existing users of same id and/or username");
                    if(getReply.failed()) {
                      logger.debug("Attempt to get users failed: " +
                              getReply.cause().getMessage());
                      asyncResultHandler.handle(Future.succeededFuture(
                                    PostUsersResponse.respond500WithTextPlain(
                                            getReply.cause().getMessage())));
                    } else {
                      List<User> userList = getReply.result().getResults();
                      if(userList.size() > 0) {
                        logger.debug("User with this id already exists");
                        asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse.respond422WithApplicationJson(
                                  ValidationHelper.createValidationErrorMessage(
                                    USER_NAME_FIELD, entity.getUsername(),
                                    "User with this id already exists"))));
                        //uh oh
                      } else {
                        PostgresClient postgresClient = PostgresClient
                                .getInstance(vertxContext.owner(), tenantId);
                        try {
                          getPG(vertxContext.owner(), tenantId, entity, handler -> {

                            int res = handler.result();
                            if(res == 0){
                              String message = "Cannot add " +
                                      entity.getPatronGroup() +
                                      ". Patron group not found";
                              logger.error(message);
                              asyncResultHandler.handle(Future.succeededFuture(
                                      PostUsersResponse.respond400WithTextPlain(
                                      message)));
                              return;
                            }
                            else if(res == -1){
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse
                                  .respond500WithTextPlain("")));
                              return;
                            }
                            else{
                              postgresClient.startTx(connection -> {
                                logger.debug("Attempting to save new record");
                                try {
                                  Date now = new Date();
                                  entity.setCreatedDate(now);
                                  entity.setUpdatedDate(now);
                                  postgresClient.save(connection, tableName, entity,
                                          reply -> {
                                    try {
                                      if(reply.succeeded()) {
                                        logger.debug("Save successful");
                                        final User user = entity;
                                        user.setId(entity.getId());
                                        postgresClient.endTx(connection, done -> {
                                          asyncResultHandler.handle(
                                                  Future.succeededFuture(
                                                    PostUsersResponse.respond201WithApplicationJson(user,
                                                     PostUsersResponse.headersFor201().withLocation(reply.result()))));
                                        });
                                      } else {
                                        postgresClient.rollbackTx(connection, rollback -> {
                                          asyncResultHandler.handle(Future.succeededFuture(
                                                  PostUsersResponse.respond400WithTextPlain(
                                                  messages.getMessage(lang,
                                                  MessageConsts.UnableToProcessRequest))));
                                        });
                                      }
                                    } catch(Exception e) {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                          PostUsersResponse.respond500WithTextPlain(
                                                  e.getMessage())));
                                    }
                                  });
                                } catch(Exception e) {
                                  postgresClient.rollbackTx(connection, rollback -> {
                                    asyncResultHandler.handle(Future.succeededFuture(
                                            PostUsersResponse.respond500WithTextPlain(
                                            getReply.cause().getMessage())));
                                  });
                                }
                              });
                            }
                          });
                        } catch (Exception e) {
                          logger.error(e.getLocalizedMessage(), e);
                          asyncResultHandler.handle(Future.succeededFuture(
                                  PostUsersResponse.respond500WithTextPlain(
                                  messages.getMessage(lang, MessageConsts.InternalServerError))));
                        }
                      }
                   }
                  });
              } catch(Exception e) {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostUsersResponse.respond500WithTextPlain(
                        messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
            }
          });
        } catch(Exception e) {
          logger.error(e.getLocalizedMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.respond500WithTextPlain(
                  messages.getMessage(lang, MessageConsts.InternalServerError))));

        }

      });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.respond500WithTextPlain(
              messages.getMessage(lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void getUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
     try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        String tableName = getTableName(null);
            try {
              Criteria idCrit = new Criteria(USER_SCHEMA_PATH);
              idCrit.addField(USER_ID_FIELD);
              idCrit.setOperation("=");
              idCrit.setValue(userId);
              Criterion criterion = new Criterion(idCrit);
              logger.debug("Using criterion: " + criterion.toString());
               PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tableName, User.class, criterion,
                       true, false, getReply -> {
                 if(getReply.failed()) {
                   asyncResultHandler.handle(Future.succeededFuture(
                           GetUsersByUserIdResponse.respond500WithTextPlain(
                                   messages.getMessage(lang, MessageConsts.InternalServerError))));
                 } else {
                   List<User> userList = getReply.result().getResults();
                   if(userList.size() < 1) {
                     asyncResultHandler.handle(Future.succeededFuture(
                            GetUsersByUserIdResponse.respond404WithTextPlain("User" +
                                    messages.getMessage(lang,
                                            MessageConsts.ObjectDoesNotExist))));
                   } else if(userList.size() > 1) {
                     logger.debug("Multiple users found with the same id");
                     asyncResultHandler.handle(Future.succeededFuture(
                          GetUsersByUserIdResponse.respond500WithTextPlain(
                                  messages.getMessage(lang,
                                          MessageConsts.InternalServerError))));
                   } else {
                     asyncResultHandler.handle(Future.succeededFuture(
                            GetUsersByUserIdResponse.respond200WithApplicationJson(userList.get(0))));
                   }
                 }
               });
             } catch(Exception e) {
               logger.debug("Error occurred: " + e.getMessage());
               asyncResultHandler.handle(Future.succeededFuture(
                      GetUsersResponse.respond500WithTextPlain(messages.getMessage(
                              lang, MessageConsts.InternalServerError))));
             }

       });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              GetUsersResponse.respond500WithTextPlain(messages.getMessage(
                      lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void deleteUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    try {
      vertxContext.runOnContext(v-> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        Criteria idCrit = new Criteria();
        idCrit.addField(USER_ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(userId);
        String tableName = getTableName(null);

            try {
              PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(
                      tableName, new Criterion(idCrit), deleteReply -> {
                if(deleteReply.failed()) {
                  logger.debug("Delete failed: " + deleteReply.cause().getMessage());
                  asyncResultHandler.handle(Future.succeededFuture(
                            DeleteUsersByUserIdResponse.respond404WithTextPlain("Not found")));
                } else {
                   asyncResultHandler.handle(Future.succeededFuture(
                            DeleteUsersByUserIdResponse.respond204()));
                }
              });
            } catch(Exception e) {
              logger.debug("Delete failed: " + e.getMessage());
              asyncResultHandler.handle(
                Future.succeededFuture(
                        DeleteUsersByUserIdResponse.respond500WithTextPlain(
                                messages.getMessage(lang,
                                        MessageConsts.InternalServerError))));
            }

      });
    } catch(Exception e) {
      asyncResultHandler.handle(
            Future.succeededFuture(
                    DeleteUsersByUserIdResponse.respond500WithTextPlain(
                            messages.getMessage(lang,
                                    MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void putUsersByUserId(String userId,
          String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    try {
      vertxContext.runOnContext(v-> {
        if(checkForDuplicateAddressTypes(entity)) {
          asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.respond400WithTextPlain("Users are limited to one address per addresstype")));
          return;
        }

        if(!userId.equals(entity.getId())) {
          asyncResultHandler.handle(Future.succeededFuture(
                          PutUsersByUserIdResponse.respond400WithTextPlain("You cannot change the value of the id field")));
        } else {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
          String tableName = getTableName(null);

          try {
            Criteria nameCrit = new Criteria(USER_SCHEMA_PATH);
            nameCrit.addField(USER_NAME_FIELD);
            nameCrit.setOperation("=");
            nameCrit.setValue(entity.getUsername());

            checkAllAddressTypesValid(entity, vertxContext, tenantId).setHandler(checkRes -> {
              if(checkRes.failed()) {
                logger.debug(checkRes.cause().getLocalizedMessage(), checkRes.cause());
                  asyncResultHandler.handle(Future.succeededFuture(
                    PutUsersByUserIdResponse.respond500WithTextPlain(
                            messages.getMessage(lang, MessageConsts.InternalServerError))));
              } else if(!checkRes.result()) {
                asyncResultHandler.handle(Future.succeededFuture(
                  PostUsersResponse.respond400WithTextPlain("All addresses types defined for users must be existing")));
              } else {
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tableName,
                          User.class, new Criterion(nameCrit), true, false, getReply -> {
                    if(getReply.failed()) {
                      //error 500
                      logger.debug("Error querying existing username: " + getReply.cause().getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(
                                              PutUsersByUserIdResponse.respond500WithTextPlain(
                                                      messages.getMessage(lang,
                                                              MessageConsts.InternalServerError))));
                    } else {
                      List<User> userList = getReply.result().getResults();
                      if(userList.size() > 0 && (!userList.get(0).getId().equals(entity.getId()))) {
                        //Error 400, that username is in use by somebody else
                        asyncResultHandler.handle(Future.succeededFuture(
                                PutUsersByUserIdResponse.respond400WithTextPlain(
                                        "Username " + entity.getUsername() + " is already in use")));
                      } else {
                        try {
                          getPG(vertxContext.owner(), tenantId, entity, handler -> {

                            int res = handler.result();
                            if(res == 0){
                              String message = "Can not add " + entity.getPatronGroup() + ". Patron group not found";
                              logger.error(message);
                              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostUsersResponse
                                .respond400WithTextPlain(message)));
                              return;
                            }
                            else if(res == -1){
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse
                                  .respond500WithTextPlain("")));
                              return;
                            }
                            else{
                              Date createdDate = null;
                              Date now = new Date();
                              if(userList.size() > 0) {
                                createdDate = userList.get(0).getCreatedDate();
                              } else {
                                createdDate = now;
                              }
                              Criteria idCrit = new Criteria();
                              idCrit.addField(USER_ID_FIELD);
                              idCrit.setOperation("=");
                              idCrit.setValue(userId);
                              entity.setUpdatedDate(now);
                              entity.setCreatedDate(createdDate);
                              try {
                                PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
                                        tableName, entity, new Criterion(idCrit), true, putReply -> {
                                  try {
                                    if(putReply.failed()) {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                              PutUsersByUserIdResponse.respond500WithTextPlain(
                                                putReply.cause().getMessage())));
                                    } else {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                              PutUsersByUserIdResponse.respond204()));
                                    }
                                  } catch(Exception e) {
                                    asyncResultHandler.handle(Future.succeededFuture(
                                                    PutUsersByUserIdResponse.respond500WithTextPlain(
                                                            messages.getMessage(lang,
                                                                    MessageConsts.InternalServerError))));
                                  }
                                });
                              } catch(Exception e) {
                                asyncResultHandler.handle(Future.succeededFuture(
                                                    PutUsersByUserIdResponse.respond500WithTextPlain(
                                                            messages.getMessage(lang,
                                                                    MessageConsts.InternalServerError))));
                              }
                            }
                          });
                        } catch (Exception e) {
                          logger.error(e.getLocalizedMessage(), e);
                          asyncResultHandler.handle(Future.succeededFuture(
                            PutUsersByUserIdResponse.respond500WithTextPlain(
                                    messages.getMessage(lang, MessageConsts.InternalServerError))));
                        }
                      }
                    }
                  });
                } catch(Exception e) {
                  logger.debug(e.getLocalizedMessage());
                  asyncResultHandler.handle(Future.succeededFuture(
                    PutUsersByUserIdResponse.respond500WithTextPlain(
                            messages.getMessage(lang, MessageConsts.InternalServerError))));
                }
              }
            });
          } catch(Exception e) {
            logger.debug(e.getLocalizedMessage());
            asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.respond500WithTextPlain(
                      messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
        }
      });
    } catch (Exception e) {
      logger.debug(e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.respond500WithTextPlain(
                      messages.getMessage(lang, MessageConsts.InternalServerError))));
    }
  }

  /**
  *
  * @param vertx
  * @param tenantId
  * @param item
  * @param handler
  * @throws Exception
  */
 private void getPG(Vertx vertx, String tenantId, User user, Handler<AsyncResult<Integer>> handler) {
   String pgId = user.getPatronGroup();
   if(pgId == null){
     //allow null patron groups so that they can be added after a record is created
     handler.handle(io.vertx.core.Future.succeededFuture(1));
   }else{
     Criterion c = new Criterion(
       new Criteria().addField(UserGroupAPI.ID_FIELD_NAME).setJSONB(false).
       setOperation("=").setValue("'"+pgId+"'"));
     /** check if the patron group exists, if not, can not add the user **/
     PostgresClient.getInstance(vertx, tenantId).get(
       UserGroupAPI.GROUP_TABLE, Usergroup.class, c, true, false, check -> {
         if(check.succeeded()){
           List<Usergroup> ug = check.result().getResults();
           if(ug.size() == 0){
             handler.handle(io.vertx.core.Future.succeededFuture(0));
           }
           else{
             handler.handle(io.vertx.core.Future.succeededFuture(1));
           }
         }
         else{
           Throwable t = check.cause();
           logger.error(t.getLocalizedMessage(), t);
           int retCode = -1;
           if(t.getLocalizedMessage().contains("invalid input syntax for uuid")){
             retCode = 0;
           }
           handler.handle(io.vertx.core.Future.succeededFuture(retCode));
         }
     });
   }
 }

 /* Get a timestamp in RFC 3339 format, in GMT time */
 private String getTimeStamp() {
   Date date = new Date();
   DateFormat gmtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
   TimeZone gmtTimeZone = TimeZone.getTimeZone("GMT");
   gmtFormat.setTimeZone(gmtTimeZone);
   return gmtFormat.format(date);
 }

  private boolean checkForDuplicateAddressTypes(User user) {
    Map<String, Integer> countMap = new HashMap<>();
    if(user.getPersonal() != null &&
      user.getPersonal().getAddresses() != null) {
      for(Address address : user.getPersonal().getAddresses()) {
        String addressTypeId = address.getAddressTypeId();
        if(addressTypeId != null) {
          boolean found = false;
          for(String key : countMap.keySet()) {
            if(key.equals(addressTypeId)) {
              Integer count = countMap.get(key);
              count = count + 1;
              countMap.put(key, count);
              found = true;
              break;
            }
          }
          if(!found) {
            countMap.put(addressTypeId, 1);
          }
        }
      }
    }
    for(Integer i : countMap.values()) {
      if(i > 1) {
        return true;
      }
    }
    return false;
  }

  private Future<Boolean> checkAddressTypeValid(String addressTypeId, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    Criterion criterion = new Criterion(
          new Criteria().addField(AddressTypeAPI.ID_FIELD_NAME).
                  setJSONB(false).setOperation("=").setValue("'" + addressTypeId + "'"));
    vertxContext.runOnContext(v -> {
      try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(AddressTypeAPI.ADDRESS_TYPE_TABLE,
                AddressType.class, criterion, true, reply -> {
          try {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              future.fail(reply.cause());
            } else {
              List<AddressType> addressTypeList = reply.result().getResults();
              if(addressTypeList.isEmpty()) {
                future.complete(false);
              } else {
                future.complete(true);
              }
            }
          } catch(Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            future.fail(e);
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        future.fail(e);
      }
    });
    return future;
  }

  private Future<Boolean> checkAllAddressTypesValid(User user, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    List<Future> futureList = new ArrayList<>();
    if(user.getPersonal() == null || user.getPersonal().getAddresses() == null) {
      future.complete(true);
      return future;
    }
    for(Address address : user.getPersonal().getAddresses()) {
      String addressTypeId = address.getAddressTypeId();
      Future addressTypeExistsFuture = checkAddressTypeValid(addressTypeId, vertxContext, tenantId);
      futureList.add(addressTypeExistsFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        boolean bad = false;
        for(Future f : futureList) {
          Boolean result = ((Future<Boolean>)f).result();
          if(!result) {
            future.complete(false);
            bad = true;
            break;
          }
        }
        if(!bad) {
          future.complete(true);
        }
      }
    });
    return future;
  }
}
