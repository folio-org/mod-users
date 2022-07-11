package org.folio.rest.impl;

import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.AddresstypeCollection;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.resource.Addresstypes;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class AddressTypeAPI implements Addresstypes {
  public static final String ADDRESS_TYPE_TABLE = "addresstype";
  public static final String ID_FIELD_NAME = "id";
  private static final Logger logger = LogManager.getLogger(AddressTypeAPI.class);

  @Validate
  @Override
  public void getAddresstypes(String query, int offset, int limit, String lang,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(ADDRESS_TYPE_TABLE, AddressType.class, AddresstypeCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, GetAddresstypesResponse.class, asyncResultHandler);
  }

  @Override
  public void postAddresstypes(String lang, AddressType entity,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
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
      PostgresClient postgresClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
      postgresClient.get(
        UsersAPI.TABLE_NAME_USERS, User.class, new String[]{"*"},
        cql, true, false, reply -> {
          if (reply.failed()) {
            String message = reply.cause().getLocalizedMessage();
            logger.error(message, reply.cause());
            asyncResultHandler.handle(Future.succeededFuture(
              DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
                message)));
          } else {
            List<User> userList = reply.result().getResults();
            if (! userList.isEmpty()) {
              String message = "Cannot remove address type '" + addresstypeId + "', " + userList.size() + " users associated with it";
              logger.error(message);
              asyncResultHandler.handle(Future.succeededFuture(DeleteAddresstypesByAddresstypeIdResponse
                .respond400WithTextPlain(message)));
              return;
            }
            logger.info("Removing non-associated address type '{}'", addresstypeId);

            PgUtil.deleteById(ADDRESS_TYPE_TABLE, addresstypeId, okapiHeaders,
              vertxContext, DeleteAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
          }
        });
    } catch (Exception e) {
      String message = e.getLocalizedMessage();
      logger.error(message, e);
      asyncResultHandler.handle(Future.succeededFuture(
        DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
          message)));
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
