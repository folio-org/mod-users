package org.folio.rest.impl;

import java.util.Map;
import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.AddresstypeCollection;
import org.folio.rest.jaxrs.resource.Addresstypes;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.service.impl.UserRepository;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class AddressTypeAPI implements Addresstypes {
  public static final String ADDRESS_TYPE_TABLE = "addresstype";
  public static final String ID_FIELD_NAME = "id";
  private static final Logger logger = LogManager.getLogger(AddressTypeAPI.class);

  private final Function<PostgresClient, UserRepository> userRepositoryFactory;

  // Used when RMB instantiates this class
  public AddressTypeAPI() {
    this(UserRepository::new);
  }

  public AddressTypeAPI(Function<PostgresClient, UserRepository> userRepositoryFactory) {
    this.userRepositoryFactory = userRepositoryFactory;
  }

  @Validate
  @Override
  public void getAddresstypes(String query, String totalRecords, int offset, int limit,
    Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.get(ADDRESS_TYPE_TABLE, AddressType.class, AddresstypeCollection.class,
      query, offset, limit, okapiHeaders, vertxContext, GetAddresstypesResponse.class, asyncResultHandler);
  }

  @Override
  public void postAddresstypes(AddressType entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.post(ADDRESS_TYPE_TABLE, entity, okapiHeaders, vertxContext,
      PostAddresstypesResponse.class, asyncResultHandler);
  }

  @Override
  public void getAddresstypesByAddresstypeId(String addresstypeId,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    PgUtil.getById(ADDRESS_TYPE_TABLE, AddressType.class, addresstypeId, okapiHeaders,
      vertxContext, GetAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
  }

  @Override
  public void deleteAddresstypesByAddresstypeId(String addresstypeId,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {

    final var userRepository = userRepositoryFactory.apply(
      PgUtil.postgresClient(vertxContext, okapiHeaders));

    //Check to make certain no users' addresses are currently using this type
    userRepository.addressTypeAssignedToUser(addresstypeId, UsersAPI::getCQL)
      .onFailure(cause -> {
        String message = cause.getLocalizedMessage();
        logger.error(message, cause);

        asyncResultHandler.handle(Future.succeededFuture(
          DeleteAddresstypesByAddresstypeIdResponse.respond500WithTextPlain(
            message)));
      })
      .onSuccess(result -> {
        if (Boolean.TRUE.equals(result)) {
          String message = "Cannot remove address type '" + addresstypeId + "' as it is being used";
          logger.error(message);
          asyncResultHandler.handle(Future.succeededFuture(DeleteAddresstypesByAddresstypeIdResponse
            .respond400WithTextPlain(message)));
          return;
        }

        logger.info("Removing non-associated address type '{}'", addresstypeId);

        PgUtil.deleteById(ADDRESS_TYPE_TABLE, addresstypeId, okapiHeaders,
          vertxContext, DeleteAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
      });
  }

  @Override
  public void putAddresstypesByAddresstypeId(String addresstypeId,
      AddressType entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.put(ADDRESS_TYPE_TABLE, entity, addresstypeId, okapiHeaders,
      vertxContext, PutAddresstypesByAddresstypeIdResponse.class, asyncResultHandler);
  }
}
