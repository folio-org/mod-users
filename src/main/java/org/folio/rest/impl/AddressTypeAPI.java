/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.resource.AddresstypesResource;

import org.folio.rest.RestVerticle;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AddresstypeCollection;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.OutStream;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.ValidationHelper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.FieldException;
/**
 *
 * @author kurt
 */
public class AddressTypeAPI implements AddresstypesResource {
  public static final String ADDRESS_TYPE_TABLE = "addresstype";
  public static final String ADDRESS_TYPE_USER_JOIN_TABLE = "address_users";
  public static final String ID_FIELD_NAME = "id";
  public static final String URL_PREFIX = "/addresstypes";
  private static final Logger logger = LoggerFactory.getLogger(AddressTypeAPI.class);
  private boolean suppressErrorResponse = false;

  public void setSuppressErrorResponse(boolean suppressErrorResponse) {
    this.suppressErrorResponse = suppressErrorResponse;
  }
  
  public AddressTypeAPI(Vertx vertx, String tenantId) {
    PostgresClient.getInstance(vertx, tenantId).setIdField(ID_FIELD_NAME);
  }
  
  private String getErrorResponse(String response) {
    if(suppressErrorResponse) {
      return "Internal Server Error: Please contact Admin";
    }
    return response;
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
  
  @Validate
  @Override
  public void getAddresstypes(String query, int offset, int limit, String lang,
          Map<String, String> okapiHeaders, 
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    vertxContext.runOnContext( v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(
                RestVerticle.OKAPI_HEADER_TENANT));
        CQLWrapper cql = getCQL(query, limit, offset, ADDRESS_TYPE_TABLE);
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(ADDRESS_TYPE_TABLE, AddressType.class,
                new String[]{"*"}, cql, true, true, reply -> {
                  try {
                    if(reply.failed()) {
                      String message = reply.cause().getLocalizedMessage();
                      logger.error(message, reply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                              GetAddresstypesResponse.withPlainBadRequest(message)));
                    } else {
                      AddresstypeCollection addresstypeCollection = new AddresstypeCollection();
                      List<AddressType> addressTypeList = (List<AddressType>)reply.result()[0];
                      addresstypeCollection.setAddressTypes(addressTypeList);
                      addresstypeCollection.setTotalRecords((Integer)reply.result()[1]);
                      asyncResultHandler.handle(Future.succeededFuture(
                              GetAddresstypesResponse.withJsonOK(addresstypeCollection)));
                      
                    }
                  } catch(Exception e) {
                    String message = e.getLocalizedMessage();
                    logger.error(message, e);
                    asyncResultHandler.handle(Future.succeededFuture(
                            GetAddresstypesResponse.withPlainInternalServerError(
                                    getErrorResponse(message))));
                  }
                });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void postAddresstypes(String lang, AddressType entity, Map<String, 
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(
                RestVerticle.OKAPI_HEADER_TENANT));
        PostgresClient.getInstance(vertxContext.owner(), tenantId).save(
                ADDRESS_TYPE_TABLE, entity, reply -> {
          try {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              if(isDuplicate(message)) {
                //Address Type already exists
                asyncResultHandler.handle(Future.succeededFuture(
                        PostAddresstypesResponse.withJsonUnprocessableEntity(
                                ValidationHelper.createValidationErrorMessage(
                                        "addresstype", entity.getAddressType(),
                                          "Address Type Exisits"))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostAddresstypesResponse.withPlainInternalServerError(
                                getErrorResponse(message))));
              }
            } else {
              Object returnObject = reply.result();
              entity.setId((String) returnObject);
              OutStream stream = new OutStream();
              stream.setData(entity);
              asyncResultHandler.handle(Future.succeededFuture(
                      PostAddresstypesResponse.withJsonCreated(
                              URL_PREFIX + returnObject, stream)));
            }
          } catch(Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            asyncResultHandler.handle(Future.succeededFuture(
                    PostAddresstypesResponse.withPlainInternalServerError(
                            getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                PostAddresstypesResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void getAddresstypesByAddresstypeId(String addresstypeId, String lang, 
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(
                okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));
        Criterion criterion = new Criterion(
          new Criteria().addField(ID_FIELD_NAME).
                  setJSONB(false).setOperation("=").setValue("'" + addresstypeId + "'"));
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(ADDRESS_TYPE_TABLE,
                AddressType.class, criterion, true, reply -> {
          try {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              if(isInvalidUUID(message)) {
                //Not found
                asyncResultHandler.handle(Future.succeededFuture(
                        GetAddresstypesByAddresstypeIdResponse.withPlainNotFound(addresstypeId)));
              } else {
                //Server Error
                asyncResultHandler.handle(Future.succeededFuture(
                  GetAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
              }
            } else {
              List<AddressType> addressTypeList = (List<AddressType>)reply.result()[0];
              if(addressTypeList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetAddresstypesByAddresstypeIdResponse.withPlainNotFound(addresstypeId)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetAddresstypesByAddresstypeIdResponse.withJsonOK(addressTypeList.get(0))));
              }
            }
          } catch(Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
      }
    });    
  }

  @Override
  public void deleteAddresstypesByAddresstypeId(String addresstypeId, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(
                okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));
        try {
          //Check to make certain no users' addresses are currently using this type
          /* CQL statement to check for users with addresses that use a particular address type */
          String query = "personal.addresses=" + addresstypeId;
          CQLWrapper cql = getCQL(query,1,0, UsersAPI.TABLE_NAME_USER);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  UsersAPI.TABLE_NAME_USER, User.class, new String[]{"*"},
                    cql, true, false, reply -> {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                      DeleteAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                              getErrorResponse(message))));
            } else {
              List<User> userList = (List<User>)reply.result()[0];
              if(userList.size() > 0) {
                String message = "Cannot remove address type '" + addresstypeId + "', " + userList.size() + " users associated with it";
                logger.error(message);
                asyncResultHandler.handle(Future.succeededFuture(DeleteAddresstypesByAddresstypeIdResponse.withPlainBadRequest(message)));
              } else {
                logger.info("Removing non-associated address type '" + addresstypeId + "'");
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(
                          ADDRESS_TYPE_TABLE, addresstypeId, deleteReply -> {
                    if(deleteReply.failed()) {
                      String message = deleteReply.cause().getLocalizedMessage();
                      logger.error(message, deleteReply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                              DeleteAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                                      getErrorResponse(message))));
                    } else {
                      if(deleteReply.result().getUpdated() == 1) {
                        asyncResultHandler.handle(Future.succeededFuture(
                              DeleteAddresstypesByAddresstypeIdResponse.withNoContent()));
                      } else {
                        String message = Messages.getInstance().getMessage(
                                lang, MessageConsts.DeletedCountError, 1, 
                                  deleteReply.result().getUpdated());
                        logger.error(message);
                        asyncResultHandler.handle(Future.succeededFuture(
                          DeleteAddresstypesByAddresstypeIdResponse.withPlainNotFound(message)));
                      }
                    }
                  });
                
                } catch(Exception e) {
                  String message = e.getLocalizedMessage();
                  asyncResultHandler.handle(Future.succeededFuture(
                          DeleteAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                                  getErrorResponse(message))));
                }
              }
            }
          });
          
        } catch(Exception e) {
          String message = e.getLocalizedMessage();
          logger.error(message, e);
          asyncResultHandler.handle(Future.succeededFuture(
                  DeleteAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                          getErrorResponse(message))));
        }
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                DeleteAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
      }
    });      
  }

  @Override
  public void putAddresstypesByAddresstypeId(String addresstypeId, String lang, AddressType entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(
                okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));
        PostgresClient.getInstance(vertxContext.owner(), tenantId).update(ADDRESS_TYPE_TABLE, entity, addresstypeId, reply -> {
          try {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                           getErrorResponse(message))));
            } else {
              if(reply.result().getUpdated() == 0) {
                String message = "No records updated";
                logger.error(message);
                asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.withPlainNotFound(message)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.withNoContent()));
              }
            }        
          } catch(Exception e) {
           String message = e.getLocalizedMessage();
           logger.error(message, e);
           asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                           getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                PutAddresstypesByAddresstypeIdResponse.withPlainInternalServerError(
                        getErrorResponse(message))));
      }
    });    
  }
  
  
  private CQLWrapper getCQL(String query, int limit, int offset, String tableName) throws FieldException{
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }
  
}
