package org.folio.rest.impl;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.RestVerticle;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.AddresstypeCollection;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.resource.Addresstypes;
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

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.UUID;
/**
 *
 * @author kurt
 */
public class AddressTypeAPI implements Addresstypes {
  public static final String ADDRESS_TYPE_TABLE = "addresstype";
  public static final String ADDRESS_TYPE_USER_JOIN_TABLE = "address_users";
  public static final String ID_FIELD_NAME = "id";
  public static final String URL_PREFIX = "/addresstypes";
  private static final Logger logger = LoggerFactory.getLogger(AddressTypeAPI.class);
  private boolean suppressErrorResponse = false;
  private static final String ADDRESS_TYPE_TABLE_SCHEMA_PATH = UsersAPI.RAML_PATH + "/schemas/mod-users/addresstype.json";
  private static final String ADDRESS_TYPE_TABLE_SCHEMA = schema(ADDRESS_TYPE_TABLE_SCHEMA_PATH);

  // TODO: replace by ResourceUtils.resource2String: https://issues.folio.org/browse/RMB-258
  private static String schema(String path) {
    try {
      return ResourceUtils.resource2String(path);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

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
          Context vertxContext) {
    vertxContext.runOnContext( v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(
                RestVerticle.OKAPI_HEADER_TENANT));
        CQLWrapper cql = getCQL(query, limit, offset);
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(ADDRESS_TYPE_TABLE, AddressType.class,
                new String[]{"*"}, cql, true, true, reply -> {
                  try {
                    if(reply.failed()) {
                      String message = reply.cause().getLocalizedMessage();
                      logger.error(message, reply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                              GetAddresstypesResponse.respond400WithTextPlain(message)));
                    } else {
                      AddresstypeCollection addresstypeCollection = new AddresstypeCollection();
                      List<AddressType> addressTypeList = reply.result().getResults();
                      addresstypeCollection.setAddressTypes(addressTypeList);
                      addresstypeCollection.setTotalRecords(reply.result().getResultInfo().getTotalRecords());
                      asyncResultHandler.handle(Future.succeededFuture(
                              GetAddresstypesResponse.respond200WithApplicationJson(addresstypeCollection)));

                    }
                  } catch(Exception e) {
                    String message = e.getLocalizedMessage();
                    logger.error(message, e);
                    asyncResultHandler.handle(Future.succeededFuture(
                            GetAddresstypesResponse.respond500WithTextPlain(
                                    getErrorResponse(message))));
                  }
                });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void postAddresstypes(String lang, AddressType entity, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(
                RestVerticle.OKAPI_HEADER_TENANT));
        String id = entity.getId();
        if(id == null) {
          id = UUID.randomUUID().toString();
          entity.setId(id);
        }
        PostgresClient.getInstance(vertxContext.owner(), tenantId).save(
                ADDRESS_TYPE_TABLE, id, entity, reply -> {
          try {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              if(isDuplicate(message)) {
                //Address Type already exists
                asyncResultHandler.handle(Future.succeededFuture(
                        PostAddresstypesResponse.respond422WithApplicationJson(
                                ValidationHelper.createValidationErrorMessage(
                                        "addresstype", entity.getAddressType(),
                                          "Address Type Exisits"))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostAddresstypesResponse.respond500WithTextPlain(
                                getErrorResponse(message))));
              }
            } else {
              String returnObject = reply.result();
              entity.setId(returnObject);
              asyncResultHandler.handle(Future.succeededFuture(
                      PostAddresstypesResponse.respond201WithApplicationJson(entity,
                        PostAddresstypesResponse.headersFor201().withLocation(
                              URL_PREFIX + returnObject))));
            }
          } catch(Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            asyncResultHandler.handle(Future.succeededFuture(
                    PostAddresstypesResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                PostAddresstypesResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void getAddresstypesByAddresstypeId(String addresstypeId, String lang,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
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
                        GetAddresstypesByAddresstypeIdResponse.respond404WithTextPlain(addresstypeId)));
              } else {
                //Server Error
                asyncResultHandler.handle(Future.succeededFuture(
                  GetAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
              }
            } else {
              List<AddressType> addressTypeList = reply.result().getResults();
              if(addressTypeList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetAddresstypesByAddresstypeIdResponse.respond404WithTextPlain(addresstypeId)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetAddresstypesByAddresstypeIdResponse.respond200WithApplicationJson(addressTypeList.get(0))));
              }
            }
          } catch(Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                GetAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void deleteAddresstypesByAddresstypeId(String addresstypeId, String lang,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    vertxContext.runOnContext(v -> {
      try {
        String tenantId = TenantTool.calculateTenantId(
                okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));
        try {
          //Check to make certain no users' addresses are currently using this type
          /* CQL statement to check for users with addresses that use a particular address type */
          String query = "personal.addresses=" + addresstypeId;
          CQLWrapper cql = UsersAPI.getCQL(query, 1, 0);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  UsersAPI.TABLE_NAME_USERS, User.class, new String[]{"*"},
                    cql, true, false, reply -> {
            if(reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                      DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                              getErrorResponse(message))));
            } else {
              List<User> userList = reply.result().getResults();
              if(userList.size() > 0) {
                String message = "Cannot remove address type '" + addresstypeId + "', " + userList.size() + " users associated with it";
                logger.error(message);
                asyncResultHandler.handle(Future.succeededFuture(DeleteAddresstypesByAddresstypeIdResponse
                  .respond400WithTextPlain(message)));
              } else {
                logger.info("Removing non-associated address type '" + addresstypeId + "'");
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(
                          ADDRESS_TYPE_TABLE, addresstypeId, deleteReply -> {
                    if(deleteReply.failed()) {
                      String message = deleteReply.cause().getLocalizedMessage();
                      logger.error(message, deleteReply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                              DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                                      getErrorResponse(message))));
                    } else {
                      if(deleteReply.result().getUpdated() == 1) {
                        asyncResultHandler.handle(Future.succeededFuture(
                              DeleteAddresstypesByAddresstypeIdResponse.respond204()));
                      } else {
                        String message = Messages.getInstance().getMessage(
                                lang, MessageConsts.DeletedCountError, 1,
                                  deleteReply.result().getUpdated());
                        logger.error(message);
                        asyncResultHandler.handle(Future.succeededFuture(
                          DeleteAddresstypesByAddresstypeIdResponse.respond404WithTextPlain(message)));
                      }
                    }
                  });

                } catch(Exception e) {
                  String message = e.getLocalizedMessage();
                  asyncResultHandler.handle(Future.succeededFuture(
                          DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                                  getErrorResponse(message))));
                }
              }
            }
          });

        } catch(Exception e) {
          String message = e.getLocalizedMessage();
          logger.error(message, e);
          asyncResultHandler.handle(Future.succeededFuture(
                  DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                          getErrorResponse(message))));
        }
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
      }
    });
  }

  @Override
  public void putAddresstypesByAddresstypeId(String addresstypeId, String lang,
    AddressType entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

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
                   PutAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                           getErrorResponse(message))));
            } else {
              if(reply.result().getUpdated() == 0) {
                String message = "No records updated";
                logger.error(message);
                asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.respond404WithTextPlain(message)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.respond204()));
              }
            }
          } catch(Exception e) {
           String message = e.getLocalizedMessage();
           logger.error(message, e);
           asyncResultHandler.handle(Future.succeededFuture(
                   PutAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                           getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        asyncResultHandler.handle(Future.succeededFuture(
                PutAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                        getErrorResponse(message))));
      }
    });
  }

  private CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException, IOException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(ADDRESS_TYPE_TABLE + ".jsonb", ADDRESS_TYPE_TABLE_SCHEMA);
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

}
