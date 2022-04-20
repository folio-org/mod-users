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

  static Response response(String message, Throwable e, String lang,
      Function<String,Response> report400, Function<String,Response> report500) {

    try {
      Throwable cause = e;
      while (cause != null) {
        if (cause instanceof CQLParseException || cause instanceof FieldException) {
          return report400.apply("CQL Parsing Error for '" + message + "': " + cause.getMessage());
        }
        if (cause instanceof IllegalStateException) {
          return report400.apply("CQL Illegal State Error for '" + message + "': " + cause.getMessage());
        }
        cause = cause.getCause();
      }
      return report500.apply(messages.getMessage(lang, MessageConsts.InternalServerError));
    } catch (Exception e2) {
      logger.error(e2.getMessage(), e2);
      return report500.apply(e2.getMessage());
    }
  }


  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("postPatronPin");

    // Using SHA-512 algorithm with HMAC, to increase the memory requirement to its maximum, making it most secure pbkdf2 option.
    // SecretKeyFactory pbkdf2KeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512") ;

    // PBEKeySpec keySpec = new PBEKeySpec(charEnteredPassword, // Input character array of password
    //                               salt, // CSPRNG, unique
    //                               150000, // Iteration count (c)
    //                               32) ; // 256 bits output hashed password

    // Computes hashed password using PBKDF2HMACSHA512 algorithm and provided PBE specs.
    // byte[] pbkdfHashedArray = pbkdf2KeyFactory.generateSecret(keySpec).getEncoded() ;

    PostPatronPinResponse.respond201();
  }

  public void deletePatronPin(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("deletePatronPin");
    DeletePatronPinResponse.respond200();
  }

  public void postPatronPinVerify(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.debug("postPatronPinVerify");
    PostPatronPinVerifyResponse.respond200();
  }

}


