package org.folio.rest.impl;
import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.jaxrs.model.ProxyforCollection;
import org.folio.rest.jaxrs.resource.Proxiesfor;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.utils.PostgresClientUtil;
import org.folio.rest.utils.ValidationHelper;
/**
 *
 * @author kurt
 */
public class ProxiesForAPI implements Proxiesfor {
  public static final String PROXY_FOR_TABLE = "proxyfor";
  public static final String ID_FIELD_NAME = "id";
  public static final String USERID_FIELD_NAME = "'userId'";
  public static final String PROXY_USERID_FIELD_NAME = "'proxyUserId'";
  public static final String URL_PREFIX = "/proxiesfor";
  private static final Logger logger = LoggerFactory.getLogger(ProxiesForAPI.class);
  private boolean suppressErrorResponse = false;

  public void setSuppressErrorResponse(boolean suppressErrorResponse) {
    this.suppressErrorResponse = suppressErrorResponse;
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

  private String logAndSaveError(Throwable err) {
    String message = err.getLocalizedMessage();
    logger.error(message, err);
    return message;
  }

  private CQLWrapper getCQL(String query, int limit, int offset) throws CQL2PgJSONException, IOException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(PROXY_FOR_TABLE + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  @Override
  public void getProxiesfor(String query,
          int offset, int limit,
          String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v -> {
      try {
        CQLWrapper cql = getCQL(query, limit, offset);
        PostgresClientUtil.getInstance(vertxContext, okapiHeaders).get(
                PROXY_FOR_TABLE, ProxiesFor.class, new String[]{"*"}, cql,
                true, true, getReply -> {
          if(getReply.failed()) {
            String message = logAndSaveError(getReply.cause());
            asyncResultHandler.handle(Future.succeededFuture(
                        GetProxiesforResponse.respond500WithTextPlain(
                                getErrorResponse(message))));
          } else {
            ProxyforCollection collection = new ProxyforCollection();
            List<ProxiesFor> proxyforList = getReply.result().getResults();
            collection.setProxiesFor(proxyforList);
            collection.setTotalRecords(getReply.result().getResultInfo().getTotalRecords());
            asyncResultHandler.handle(Future.succeededFuture(
                    GetProxiesforResponse.respond200WithApplicationJson(collection)));
          }
        });
      } catch(Exception e) {
        String message = logAndSaveError(e);
        asyncResultHandler.handle(Future.succeededFuture(
                    GetProxiesforResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
      }
    });
  }

  @Override
  public void postProxiesfor(
          String lang,
          ProxiesFor entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v -> {
      try {
        PostgresClient postgresClient = PostgresClientUtil.getInstance(vertxContext, okapiHeaders);
        userAndProxyUserComboExists(entity.getUserId(), entity.getProxyUserId(),
                postgresClient, vertxContext).setHandler(existsRes -> {
          if(existsRes.failed()) {
            String message = logAndSaveError(existsRes.cause());
            asyncResultHandler.handle(Future.succeededFuture(
                    PostProxiesforResponse.respond500WithTextPlain(
                              getErrorResponse(message))));
          } else if(existsRes.result() == true) {
            Errors existsError = ValidationHelper.createValidationErrorMessage(
                              "proxyFor", entity.getId(), "Proxy relationship already exists");
                      asyncResultHandler.handle(Future.succeededFuture(
                              PostProxiesforResponse.respond422WithApplicationJson(existsError)));
          } else {
            try {
              String id = entity.getId();
              if(id == null) {
                id = UUID.randomUUID().toString();
                entity.setId(id);
              }
              postgresClient.save(PROXY_FOR_TABLE, id, entity, reply -> {
                try {
                  if(reply.failed()) {
                    String message = logAndSaveError(reply.cause());
                    if(isDuplicate(message)) {
                      Errors existsError = ValidationHelper.createValidationErrorMessage(
                              "proxyFor", entity.getId(), "Proxy relationship already exists");
                      asyncResultHandler.handle(Future.succeededFuture(
                              PostProxiesforResponse.respond422WithApplicationJson(existsError)));
                    } else {
                      asyncResultHandler.handle(Future.succeededFuture(
                              PostProxiesforResponse.respond500WithTextPlain(
                                      getErrorResponse(message))));
                    }
                  } else {
                    String returnObject = reply.result();
                    entity.setId(returnObject);
                    asyncResultHandler.handle(Future.succeededFuture(
                      PostProxiesforResponse.respond201WithApplicationJson(entity,
                        PostProxiesforResponse.headersFor201().withLocation(URL_PREFIX + returnObject))));
                  }
                } catch(Exception e) {
                  String message = logAndSaveError(e);
                  asyncResultHandler.handle(Future.succeededFuture(
                              PostProxiesforResponse.respond500WithTextPlain(
                                      getErrorResponse(message))));
                }
              });
            } catch(Exception e) {
              String message = logAndSaveError(e);
              asyncResultHandler.handle(Future.succeededFuture(
                      PostProxiesforResponse.respond500WithTextPlain(
                              getErrorResponse(message))));
            }
          }
        });
      } catch(Exception e) {
        String message = logAndSaveError(e);
        asyncResultHandler.handle(Future.succeededFuture(
                    PostProxiesforResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
      }
    });
  }

  @Override
  public void getProxiesforById(String id,
          String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v -> {
      try {
        Criterion criterion = new Criterion(new Criteria().addField(ID_FIELD_NAME)
          .setJSONB(false).setOperation("=").setVal(id));
        PostgresClientUtil.getInstance(vertxContext, okapiHeaders).get(
                PROXY_FOR_TABLE, ProxiesFor.class, criterion, true, getReply-> {
          try {
            if(getReply.failed()) {
              String message = logAndSaveError(getReply.cause());
              if(isInvalidUUID(message)) {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetProxiesforByIdResponse.respond404WithTextPlain(id)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                            GetProxiesforByIdResponse.respond500WithTextPlain(
                                    getErrorResponse(message))));
              }
            } else {
              List<ProxiesFor> proxyforList = getReply.result().getResults();
              if(proxyforList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetProxiesforByIdResponse.respond404WithTextPlain(id)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetProxiesforByIdResponse.respond200WithApplicationJson(proxyforList.get(0))));
              }
            }
          } catch(Exception e) {
            String message = logAndSaveError(e);
            asyncResultHandler.handle(Future.succeededFuture(
                        GetProxiesforByIdResponse.respond500WithTextPlain(
                                getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = logAndSaveError(e);
        asyncResultHandler.handle(Future.succeededFuture(
                    GetProxiesforByIdResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
      }
    });
  }

  @Override
  public void deleteProxiesforById(String id,
          String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v->{
      try {
        PostgresClientUtil.getInstance(vertxContext, okapiHeaders).delete(PROXY_FOR_TABLE, id, deleteReply -> {
          if(deleteReply.failed()) {
            String message = logAndSaveError(deleteReply.cause());
            asyncResultHandler.handle(Future.succeededFuture(
                        DeleteProxiesforByIdResponse.respond500WithTextPlain(
                                getErrorResponse(message))));
          } else {
            if(deleteReply.result().getUpdated() == 1) {
              asyncResultHandler.handle(Future.succeededFuture(
                      DeleteProxiesforByIdResponse.respond204()));
            } else {
              String message = Messages.getInstance().getMessage(
                      lang, MessageConsts.DeletedCountError,
                      1, deleteReply.result().getUpdated());
              logger.error(message);
              asyncResultHandler.handle(Future.succeededFuture(
                      DeleteProxiesforByIdResponse.respond404WithTextPlain(message)));
            }
          }
        });
      } catch(Exception e) {
        String message = logAndSaveError(e);
        asyncResultHandler.handle(Future.succeededFuture(
                    DeleteProxiesforByIdResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
      }
    });
  }

  @Override
  public void putProxiesforById(String id,
          String lang,
          ProxiesFor entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
    vertxContext.runOnContext(v -> {
      try {
        PostgresClientUtil.getInstance(vertxContext, okapiHeaders).update(
                PROXY_FOR_TABLE, entity, id, putReply -> {
          try {
            if(putReply.failed()) {
              String message = logAndSaveError(putReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                          PutProxiesforByIdResponse.respond500WithTextPlain(
                                  getErrorResponse(message))));
            } else {
              if(putReply.result().getUpdated() == 0) {
                String message = "No records updated";
                logger.error(message);
                asyncResultHandler.handle(
                        Future.succeededFuture(
                                PutProxiesforByIdResponse.respond404WithTextPlain(message)));
              } else {
                asyncResultHandler.handle(
                        Future.succeededFuture(PutProxiesforByIdResponse.respond204()));
              }
            }
          } catch(Exception e) {
            String message = logAndSaveError(e);
            asyncResultHandler.handle(Future.succeededFuture(
                        PutProxiesforByIdResponse.respond500WithTextPlain(
                                getErrorResponse(message))));
          }
        });
      } catch(Exception e) {
        String message = logAndSaveError(e);
        asyncResultHandler.handle(Future.succeededFuture(
                    PutProxiesforByIdResponse.respond500WithTextPlain(
                            getErrorResponse(message))));
      }
    });
  }

  private Future<Boolean> userAndProxyUserComboExists(
          String userId,
          String proxyUserId,
          PostgresClient postgresClient,
          Context vertxContext) {
    Future<Boolean> future = Future.future();
    vertxContext.runOnContext(v -> {
      try {
        Criteria userCrit = new Criteria().addField(USERID_FIELD_NAME).
                setOperation("=").setVal(userId).setJSONB(true);
        Criteria proxyUserCrit = new Criteria().addField(PROXY_USERID_FIELD_NAME).
                setOperation("=").setVal(proxyUserId).setJSONB(true);
        Criterion criterion = new Criterion();
        criterion.addCriterion(userCrit, "AND", proxyUserCrit);
        postgresClient.get(PROXY_FOR_TABLE, ProxiesFor.class, criterion, true, getReply -> {
          try {
            if(getReply.failed()) {
              future.fail(getReply.cause());
            } else {
              List<ProxiesFor> proxyForList = getReply.result().getResults();
              if(proxyForList.isEmpty()) {
                future.complete(false);
              } else {
                future.complete(true);
              }
            }
          } catch(Exception e) {
            future.fail(e);
          }
        });
      } catch(Exception e) {
        future.fail(e);
      }
    });
    return future;
  }

}
