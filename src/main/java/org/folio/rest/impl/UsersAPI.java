package org.folio.rest.impl;

import java.util.List;
import java.util.Map;
import java.util.Date;
import java.util.HashMap;
import java.util.TimeZone;
import java.util.ArrayList;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.UserdataCollection;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.resource.UsersResource;
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
import io.vertx.core.CompositeFuture;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;


/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements UsersResource {

  public static final String TABLE_NAME_USER = "users";

  private final Messages messages = Messages.getInstance();
  //private final String USER_COLLECTION = "user";
  private static final String USER_ID_FIELD = "'id'";
  private static final String USER_NAME_FIELD = "'username'";
  private static final String OKAPI_HEADER_TENANT = "x-okapi-tenant";
  private final Logger logger = LoggerFactory.getLogger(UsersAPI.class);


  public UsersAPI(Vertx vertx, String tenantId) {
    PostgresClient.getInstance(vertx, tenantId).setIdField("id");
  }

  private String getTableName(String tenantId, String tableBase) {
    //This hardly deserves to be a method, but since details may change, I'm
    //trying to keep it flexible
    //return tenantId + "." + tableBase;
    return tableBase;
  }


  private CQLWrapper getCQL(String query, int limit, int offset) throws FieldException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(TABLE_NAME_USER+".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  @Validate
  @Override
  public void getUsers(String query, String orderBy,
          Order order, int offset, int limit, String lang,
          Map <String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    logger.debug("Getting users");
    try {
      CQLWrapper cql = getCQL(query,limit,offset);
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        String tableName = getTableName(tenantId, TABLE_NAME_USER);
        String[] fieldList = {"*"};
        //Criterion criterion = Criterion.json2Criterion(query);
        //criterion.setLimit(new Limit(limit)).setOffset(new Offset(offset));
        logger.debug("Headers present are: " + okapiHeaders.keySet().toString());
        //logger.debug("Using criterion: " + criterion.toString());
        logger.debug("tenantId = " + tenantId);

            try {
              PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tableName,
                      User.class, fieldList, cql, true, false, reply -> {
                try {
                  if(reply.succeeded()) {
                    UserdataCollection userCollection = new UserdataCollection();
                    List<User> users = (List<User>)reply.result()[0];
                    userCollection.setUsers(users);
                    userCollection.setTotalRecords((Integer)reply.result()[1]);
                    asyncResultHandler.handle(Future.succeededFuture(
                            GetUsersResponse.withJsonOK(userCollection)));
                  } else {
                    asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                            GetUsersResponse.withPlainInternalServerError(
                                    reply.cause().getMessage())));
                  }
                } catch(Exception e) {
                  logger.debug(e.getLocalizedMessage());

                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                            GetUsersResponse.withPlainInternalServerError(
                                    reply.cause().getMessage())));
                }
              });
            } catch (IllegalStateException e) {
              logger.debug("IllegalStateException: " + e.getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withPlainBadRequest(
                        "CQL Illegal State Error for '" + query + "': " + e.getLocalizedMessage())));
            }
              catch(Exception e) {
              Throwable cause = e;
              while(cause.getCause() != null) {
                  cause = cause.getCause();
              }
              logger.debug("Got error " + cause.getClass().getSimpleName() + ": " + e.getLocalizedMessage());
              if(cause.getClass().getSimpleName().contains("CQLParseException")) {
                logger.debug("BAD CQL");
                asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + cause.getLocalizedMessage())));
              } else {
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                              GetUsersResponse.withPlainInternalServerError(
                                      messages.getMessage(lang,
                                              MessageConsts.InternalServerError))));
              }
            }
        });
    }
    catch(FieldException fe){
      logger.error("BAD CQL " + fe.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withPlainBadRequest(
              "CQL Parsing Error for '" + query + "': " + fe.getLocalizedMessage())));
    }
    catch(Exception e) {
      logger.error(e.getLocalizedMessage(), e);
      if(e.getCause() != null && e.getCause().getClass().getSimpleName().contains("CQLParseException")) {
        logger.debug("BAD CQL");
        asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withPlainBadRequest(
                "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
      } else {
        asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                          GetUsersResponse.withPlainInternalServerError(
                                  messages.getMessage(lang,
                                          MessageConsts.InternalServerError))));
      }
    }
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext( v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        Criteria idCrit = new Criteria();
        idCrit.addField(USER_ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(entity.getId());
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(entity.getUsername());
        Criterion crit = new Criterion();
        crit.addCriterion(idCrit, "OR", nameCrit);
        String tableName = getTableName(tenantId, TABLE_NAME_USER);
        if(checkForDuplicateAddressTypes(entity)) {
          asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.withPlainBadRequest("Users are limited to one address per addresstype")));
          return;
        }
        try {
          checkAllAddressTypesValid(entity, vertxContext, tenantId).setHandler(checkRes -> {
            if(checkRes.failed()) {
              logger.error(checkRes.cause().getLocalizedMessage(), checkRes.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.withPlainInternalServerError(
                  messages.getMessage(lang, MessageConsts.InternalServerError))));
            } else if(checkRes.result() == false) {
              asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.withPlainBadRequest("You cannot add addresses with non-existant address types")));
            } else {             
              try {
                PostgresClient.getInstance(vertxContext.owner(), TenantTool.calculateTenantId(tenantId)).get(tableName,
                        User.class, crit, true, getReply -> {
                    logger.debug("Attempting to get existing users of same id and/or username");
                    if(getReply.failed()) {
                      logger.debug("Attempt to get users failed: " + getReply.cause().getMessage());
                      asyncResultHandler.handle(Future.succeededFuture(
                                    PostUsersResponse.withPlainInternalServerError(
                                            getReply.cause().getMessage())));
                    } else {
                      List<User> userList = (List<User>)getReply.result()[0];
                      if(userList.size() > 0) {
                        logger.debug("User with this id already exists");
                        asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse.withJsonUnprocessableEntity(
                                  ValidationHelper.createValidationErrorMessage(
                                    USER_NAME_FIELD, entity.getUsername(),
                                    "User with this id already exists"))));
                        //uh oh
                      } else {
                        PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                        try {
                          getPG(vertxContext.owner(), tenantId, entity, handler -> {

                            int res = handler.result();
                            if(res == 0){
                              String message = "Can not add " + entity.getPatronGroup() + ". Patron group not found";
                              logger.error(message);
                              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostUsersResponse
                                .withPlainBadRequest(message)));
                              return;
                            }
                            else if(res == -1){
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse
                                  .withPlainInternalServerError("")));
                              return;
                            }
                            else{
                              postgresClient.startTx(beginTx -> {
                                logger.debug("Attempting to save new record");
                                try {
                                  Date now = new Date();
                                  entity.setCreatedDate(now);
                                  entity.setUpdatedDate(now);
                                  postgresClient.save(beginTx, tableName, entity, reply -> {
                                    try {
                                      if(reply.succeeded()) {
                                        logger.debug("Save successful");
                                        final User user = entity;
                                        user.setId(entity.getId());
                                        OutStream stream = new OutStream();
                                        stream.setData(user);
                                        postgresClient.endTx(beginTx, done -> {
                                          asyncResultHandler.handle(Future.succeededFuture(PostUsersResponse.withJsonCreated(reply.result(), stream)));
                                        });
                                      } else {
                                        asyncResultHandler.handle(Future.succeededFuture(
                                                PostUsersResponse.withPlainBadRequest(
                                                        messages.getMessage(
                                                                lang, MessageConsts.UnableToProcessRequest))));

                                      }
                                    } catch(Exception e) {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                          PostUsersResponse.withPlainInternalServerError(
                                                  e.getMessage())));
                                    }
                                  });
                                } catch(Exception e) {
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          PostUsersResponse.withPlainInternalServerError(
                                                  getReply.cause().getMessage())));
                                }
                              });
                            }
                          });
                        } catch (Exception e) {
                          logger.error(e.getLocalizedMessage(), e);
                          asyncResultHandler.handle(Future.succeededFuture(
                            PutUsersByUserIdResponse.withPlainInternalServerError(
                                    messages.getMessage(lang, MessageConsts.InternalServerError))));
                        }
                      }
                   }
                  });
              } catch(Exception e) {
                asyncResultHandler.handle(Future.succeededFuture(
                              PostUsersResponse.withPlainInternalServerError(
                              messages.getMessage(lang, MessageConsts.InternalServerError))));
              }
            }
          });
        } catch(Exception e) {
          logger.error(e.getLocalizedMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(
                PostUsersResponse.withPlainInternalServerError(
                  messages.getMessage(lang, MessageConsts.InternalServerError))));

        }

      });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.withPlainInternalServerError(
              messages.getMessage(lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void getUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
     try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        Criteria idCrit = new Criteria();
        idCrit.addField(USER_ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(userId);
        Criterion criterion = new Criterion(idCrit);
        logger.debug("Using criterion: " + criterion.toString());
        String tableName = getTableName(tenantId, TABLE_NAME_USER);

            try {
               PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tableName, User.class, criterion,
                       true, false, getReply -> {
                 if(getReply.failed()) {
                   asyncResultHandler.handle(Future.succeededFuture(
                           GetUsersByUserIdResponse.withPlainInternalServerError(
                                   messages.getMessage(lang, MessageConsts.InternalServerError))));
                 } else {
                   List<User> userList = (List<User>)getReply.result()[0];
                   if(userList.size() < 1) {
                     asyncResultHandler.handle(Future.succeededFuture(
                            GetUsersByUserIdResponse.withPlainNotFound("User" +
                                    messages.getMessage(lang,
                                            MessageConsts.ObjectDoesNotExist))));
                   } else if(userList.size() > 1) {
                     logger.debug("Multiple users found with the same id");
                     asyncResultHandler.handle(Future.succeededFuture(
                          GetUsersByUserIdResponse.withPlainInternalServerError(
                                  messages.getMessage(lang,
                                          MessageConsts.InternalServerError))));
                   } else {
                     asyncResultHandler.handle(Future.succeededFuture(
                            GetUsersByUserIdResponse.withJsonOK(userList.get(0))));
                   }
                 }
               });
             } catch(Exception e) {
               logger.debug("Error occurred: " + e.getMessage());
               asyncResultHandler.handle(Future.succeededFuture(
                      GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                              lang, MessageConsts.InternalServerError))));
             }

       });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                      lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void deleteUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v-> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
        Criteria idCrit = new Criteria();
        idCrit.addField(USER_ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(userId);
        String tableName = getTableName(tenantId, TABLE_NAME_USER);

            try {
              PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(
                      tableName, new Criterion(idCrit), deleteReply -> {
                if(deleteReply.failed()) {
                  logger.debug("Delete failed: " + deleteReply.cause().getMessage());
                  asyncResultHandler.handle(Future.succeededFuture(
                            DeleteUsersByUserIdResponse.withPlainNotFound("Not found")));
                } else {
                   asyncResultHandler.handle(Future.succeededFuture(
                            DeleteUsersByUserIdResponse.withNoContent()));
                }
              });
            } catch(Exception e) {
              logger.debug("Delete failed: " + e.getMessage());
              asyncResultHandler.handle(
                Future.succeededFuture(
                        DeleteUsersByUserIdResponse.withPlainInternalServerError(
                                messages.getMessage(lang,
                                        MessageConsts.InternalServerError))));
            }

      });
    } catch(Exception e) {
      asyncResultHandler.handle(
            Future.succeededFuture(
                    DeleteUsersByUserIdResponse.withPlainInternalServerError(
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
          Context vertxContext) throws Exception {


    try {
      vertxContext.runOnContext(v-> {
        if(checkForDuplicateAddressTypes(entity)) {
          asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.withPlainBadRequest("Users are limited to one address per addresstype")));
          return;
        }

        if(!userId.equals(entity.getId())) {
          asyncResultHandler.handle(Future.succeededFuture(
                          PutUsersByUserIdResponse.withPlainBadRequest("You cannot change the value of the id field")));
        } else {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
          String tableName = getTableName(tenantId, TABLE_NAME_USER);
          Criteria nameCrit = new Criteria();
          nameCrit.addField(USER_NAME_FIELD);
          nameCrit.setOperation("=");
          nameCrit.setValue(entity.getUsername());
          try {
            checkAllAddressTypesValid(entity, vertxContext, tenantId).setHandler(checkRes -> {
              if(checkRes.failed()) {
                logger.debug(checkRes.cause().getLocalizedMessage(), checkRes.cause());
                  asyncResultHandler.handle(Future.succeededFuture(
                    PutUsersByUserIdResponse.withPlainInternalServerError(
                            messages.getMessage(lang, MessageConsts.InternalServerError))));
              } else if(!checkRes.result()) {
                asyncResultHandler.handle(Future.succeededFuture(
                  PostUsersResponse.withPlainBadRequest("All addresses types defined for users must be existing")));
              } else {
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tableName,
                          User.class, new Criterion(nameCrit), true, false, getReply -> {
                    if(getReply.failed()) {
                      //error 500
                      logger.debug("Error querying existing username: " + getReply.cause().getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(
                                              PutUsersByUserIdResponse.withPlainInternalServerError(
                                                      messages.getMessage(lang,
                                                              MessageConsts.InternalServerError))));
                    } else {
                      List<User> userList = (List<User>)getReply.result()[0];
                      if(userList.size() > 0 && (!userList.get(0).getId().equals(entity.getId()))) {
                        //Error 400, that username is in use by somebody else
                        asyncResultHandler.handle(Future.succeededFuture(
                                PutUsersByUserIdResponse.withPlainBadRequest(
                                        "Username " + entity.getUsername() + " is already in use")));
                      } else {
                        try {
                          getPG(vertxContext.owner(), tenantId, entity, handler -> {

                            int res = handler.result();
                            if(res == 0){
                              String message = "Can not add " + entity.getPatronGroup() + ". Patron group not found";
                              logger.error(message);
                              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(PostUsersResponse
                                .withPlainBadRequest(message)));
                              return;
                            }
                            else if(res == -1){
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostUsersResponse
                                  .withPlainInternalServerError("")));
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
                                              PutUsersByUserIdResponse.withPlainInternalServerError(putReply.cause().getMessage())));
                                    } else {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                              PutUsersByUserIdResponse.withNoContent()));
                                    }
                                  } catch(Exception e) {
                                    asyncResultHandler.handle(Future.succeededFuture(
                                                    PutUsersByUserIdResponse.withPlainInternalServerError(
                                                            messages.getMessage(lang,
                                                                    MessageConsts.InternalServerError))));
                                  }
                                });
                              } catch(Exception e) {
                                asyncResultHandler.handle(Future.succeededFuture(
                                                    PutUsersByUserIdResponse.withPlainInternalServerError(
                                                            messages.getMessage(lang,
                                                                    MessageConsts.InternalServerError))));
                              }
                            }
                          });
                        } catch (Exception e) {
                          logger.error(e.getLocalizedMessage(), e);
                          asyncResultHandler.handle(Future.succeededFuture(
                            PutUsersByUserIdResponse.withPlainInternalServerError(
                                    messages.getMessage(lang, MessageConsts.InternalServerError))));
                        }
                      }
                    }
                  });
                } catch(Exception e) {
                  logger.debug(e.getLocalizedMessage());
                  asyncResultHandler.handle(Future.succeededFuture(
                    PutUsersByUserIdResponse.withPlainInternalServerError(
                            messages.getMessage(lang, MessageConsts.InternalServerError))));
                }
              }
            });
          } catch(Exception e) {
            logger.debug(e.getLocalizedMessage());
            asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.withPlainInternalServerError(
                      messages.getMessage(lang, MessageConsts.InternalServerError))));
          }
        }
      });
    } catch (Exception e) {
      logger.debug(e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.withPlainInternalServerError(
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
 private void getPG(Vertx vertx, String tenantId, User user, Handler<AsyncResult<Integer>> handler) throws Exception{
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
           List<Usergroup> ug = (List<Usergroup>) check.result()[0];
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
              List<AddressType> addressTypeList = (List<AddressType>)reply.result()[0];
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

