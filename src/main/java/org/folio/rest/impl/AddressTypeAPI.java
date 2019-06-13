package org.folio.rest.impl;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.AddresstypeCollection;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.resource.Addresstypes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.utils.PostgresClientUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

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

  private String getErrorResponse(String response) {
    if (suppressErrorResponse) {
      return "Internal Server Error: Please contact Admin";
    }
    return response;
  }

  @Validate
  @Override
  public void getAddresstypes(String query, int offset, int limit, String lang,
    Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.get(ADDRESS_TYPE_TABLE, AddressType.class, AddresstypeCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, GetAddresstypesResponse.class, asyncResultHandler);
  }

  @Override
  public void postAddresstypes(String lang, AddressType entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.post(ADDRESS_TYPE_TABLE, entity, okapiHeaders, vertxContext,
      PostAddresstypesResponse.class, asyncResultHandler);
  }

  @Override
  public void getAddresstypesByAddresstypeId(String addresstypeId, String lang,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {
    PgUtil.getById(ADDRESS_TYPE_TABLE, AddressType.class, addresstypeId, okapiHeaders,
      vertxContext, GetAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
  }

  @Override
  public void deleteAddresstypesByAddresstypeId(String addresstypeId, String lang,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
    Context vertxContext) {

    try {
      //Check to make certain no users' addresses are currently using this type
      /* CQL statement to check for users with addresses that use a particular address type */
      String query = "personal.addresses=" + addresstypeId;
      CQLWrapper cql = UsersAPI.getCQL(query, 1, 0);
      PostgresClient postgresClient = PostgresClientUtil.getInstance(vertxContext, okapiHeaders);
      postgresClient.get(
        UsersAPI.TABLE_NAME_USERS, User.class, new String[]{"*"},
        cql, true, false, reply -> {
          if (reply.failed()) {
            String message = reply.cause().getLocalizedMessage();
            logger.error(message, reply.cause());
            asyncResultHandler.handle(Future.succeededFuture(
              DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                getErrorResponse(message))));
          } else {
            List<User> userList = reply.result().getResults();
            if (userList.size() > 0) {
              String message = "Cannot remove address type '" + addresstypeId + "', " + userList.size() + " users associated with it";
              logger.error(message);
              asyncResultHandler.handle(Future.succeededFuture(DeleteAddresstypesByAddresstypeIdResponse
                .respond400WithTextPlain(message)));
            } else {
              logger.info("Removing non-associated address type '" + addresstypeId + "'");

              PgUtil.deleteById(ADDRESS_TYPE_TABLE, addresstypeId, okapiHeaders,
                vertxContext, DeleteAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
            }
          }
        });
    } catch (Exception e) {
      String message = e.getLocalizedMessage();
      logger.error(message, e);
      asyncResultHandler.handle(Future.succeededFuture(
        DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
          getErrorResponse(message))));
    }
  }

  @Override
  public void putAddresstypesByAddresstypeId(String addresstypeId, String lang,
    AddressType entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.put(ADDRESS_TYPE_TABLE, entity, addresstypeId, okapiHeaders,
      vertxContext, PutAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
  }

}
