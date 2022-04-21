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

//https://github.com/folio-org/raml-module-builder/blob/10d34b51d42e038d675430da4d94bcdea9558c52/domain-models-runtime/src/main/java/org/folio/rest/persist/PostgresClient.java
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
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import io.vertx.ext.web.RoutingContext;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;


public class PatronPinAPI implements PatronPin {

  private static final Messages messages = Messages.getInstance();
  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);
  public static final String TABLE_NAME_PATRON_PIN = "patronpin";

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("postPatronPin "+entity.toString());

    String derived_key = getDerivation(entity.getPin(), entity.getId());
    entity.setPin(derived_key);

    PostgresClient pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
    Future f = pgClient.save(TABLE_NAME_PATRON_PIN, entity.getId(), entity, false, true);
    f.onComplete(  ar -> {
      logger.info("Done saving : "+ar.toString());
    });

    asyncResultHandler.handle(Future.succeededFuture( PostPatronPinResponse.respond201()));
  }

  public void deletePatronPin(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("deletePatronPin");
    asyncResultHandler.handle(Future.succeededFuture( DeletePatronPinResponse.respond200()));
  }

  public void postPatronPinVerify(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("postPatronPinVerify");
    PostgresClient pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    String supplied_pin_derivation = getDerivation(entity.getPin(), entity.getId());

    Future f = pgClient.getById(TABLE_NAME_PATRON_PIN, entity.getId());
    f.onComplete( res -> {
      io.vertx.core.AsyncResult ar = (io.vertx.core.AsyncResult) res;
      if (ar.succeeded()) {
        logger.info("Done get : "+ar.toString()+" compare key with "+supplied_pin_derivation);

        JsonObject jo = (JsonObject) ar.result();
        if ( jo.getString("pin").equals(supplied_pin_derivation) ) {
          logger.info("Pins match");
          asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond200()));
        }
        else {
          logger.info("Pins do not match");
          asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond422()));
        }
      } else {
      }
    });
    
  }

  private String getDerivation(String input, String salt) {
    String result = null;
    try {
      SecretKeyFactory pbkdf2KeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512") ;
      PBEKeySpec keySpec = new PBEKeySpec(input.toCharArray(), // Input character array of password
                                          salt.getBytes(), // We should add tenant is here also?
                                          150000, // Iteration count (c)
                                          64) ; // 256 bits output hashed password

      // Computes hashed password using PBKDF2HMACSHA512 algorithm and provided PBE specs.
      byte[] pbkdfHashedArray = pbkdf2KeyFactory.generateSecret(keySpec).getEncoded() ;
      result = javax.xml.bind.DatatypeConverter.printHexBinary(pbkdfHashedArray);
      logger.info("Generated as "+result);
    }
    catch ( java.security.NoSuchAlgorithmException nsae ) {
      // reactive handler
      logger.error("Unable to encode pin",nsae);
    }
    catch ( java.security.spec.InvalidKeySpecException ikse ) {
      logger.error("Unable to encode pin",ikse);
    }

    return result;
  }
}


