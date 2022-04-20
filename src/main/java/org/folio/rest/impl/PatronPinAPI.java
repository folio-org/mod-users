package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableObject;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.rest.annotations.Validate;

import org.folio.rest.jaxrs.resource.PatronPin;
import org.folio.rest.jaxrs.model.Patronpin;

import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.facets.FacetField;
import org.folio.rest.persist.facets.FacetManager;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import org.folio.rest.utils.ExpirationTool;
import org.folio.validate.CustomFieldValidationException;
import org.folio.validate.ValidationServiceImpl;
import org.folio.okapi.common.GenericCompositeFuture;
import org.z3950.zing.cql.CQLParseException;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import io.vertx.ext.web.RoutingContext;




public class PatronPinAPI implements PatronPin {

  private static final Messages messages = Messages.getInstance();
  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("postPatronPin");

    // Using SHA-512 algorithm with HMAC, to increase the memory requirement to its maximum, making it most secure pbkdf2 option.
    // SecretKeyFactory pbkdf2KeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512") ;

    // PBEKeySpec keySpec = new PBEKeySpec(charEnteredPassword, // Input character array of password
    //                               salt, // CSPRNG, unique
    //                               150000, // Iteration count (c)
    //                               32) ; // 256 bits output hashed password

    // Computes hashed password using PBKDF2HMACSHA512 algorithm and provided PBE specs.
    // byte[] pbkdfHashedArray = pbkdf2KeyFactory.generateSecret(keySpec).getEncoded() ;

    asyncResultHandler.handle(Future.succeededFuture( PostPatronPinResponse.respond201()));
  }

  public void deletePatronPin(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("deletePatronPin");
    asyncResultHandler.handle(Future.succeededFuture( DeletePatronPinResponse.respond200()));
  }

  public void postPatronPinVerify(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("postPatronPinVerify");
    asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond200()));
  }


  private Future<Boolean> checkPinExistsForUser(String userId, Context vertxContext, PostgresClient postgresClient) {

    Promise<Boolean> promise = Promise.promise();
    promise.complete(true);

    /*
    Criterion criterion = new Criterion(
          new Criteria().addField(AddressTypeAPI.ID_FIELD_NAME).
                  setJSONB(false).setOperation("=").setVal(addressTypeId));
    vertxContext.runOnContext(v -> {
      try {
        postgresClient.get(AddressTypeAPI.ADDRESS_TYPE_TABLE, AddressType.class, criterion, true, reply -> {
          try {
            if (reply.failed()) {
              String message = reply.cause().getLocalizedMessage();
              logger.error(message, reply.cause());
              promise.fail(reply.cause());
            } else {
              List<AddressType> addressTypeList = reply.result().getResults();
              promise.complete(!addressTypeList.isEmpty());
            }
          } catch (Exception e) {
            String message = e.getLocalizedMessage();
            logger.error(message, e);
            promise.fail(e);
          }
        });
      } catch (Exception e) {
        String message = e.getLocalizedMessage();
        logger.error(message, e);
        promise.fail(e);
      }
    });
    */
    return promise.future();
  }

}


