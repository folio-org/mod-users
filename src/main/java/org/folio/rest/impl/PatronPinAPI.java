package org.folio.rest.impl;

import javax.ws.rs.core.Response;

import org.folio.rest.jaxrs.resource.PatronPin;
import org.folio.rest.jaxrs.model.Patronpin;

import org.folio.rest.persist.PgUtil;

//https://github.com/folio-org/raml-module-builder/blob/10d34b51d42e038d675430da4d94bcdea9558c52/domain-models-runtime/src/main/java/org/folio/rest/persist/PostgresClient.java
import org.folio.rest.persist.PostgresClient;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

import java.util.Map;


public class PatronPinAPI implements PatronPin {

  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);
  public static final String TABLE_NAME_PATRON_PIN = "patronpin";

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    String derivedKey = getDerivation(entity.getPin(), entity.getId());
    entity.setPin(derivedKey);

    PostgresClient pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
    Future<String> f = pgClient.save(TABLE_NAME_PATRON_PIN, entity.getId(), entity, false, true);

    f.onComplete( res -> {
      io.vertx.core.AsyncResult ar = (io.vertx.core.AsyncResult) res;
      if (ar.succeeded()) {
        asyncResultHandler.handle(Future.succeededFuture( PostPatronPinResponse.respond201()));
      }
      else {
        asyncResultHandler.handle(Future.succeededFuture( PostPatronPinResponse.respond500WithTextPlain("Failed to set Pin "+ar.toString())));
      }
    });
  }

  public void deletePatronPin(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PostgresClient pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);
    Future<io.vertx.sqlclient.RowSet<io.vertx.sqlclient.Row>> f = pgClient.delete(TABLE_NAME_PATRON_PIN, entity.getId());

    f.onComplete( res -> {
      io.vertx.core.AsyncResult ar = (io.vertx.core.AsyncResult) res;
      if (ar.succeeded()) {
        asyncResultHandler.handle(Future.succeededFuture( DeletePatronPinResponse.respond200()));
      }
      else {
        asyncResultHandler.handle(Future.succeededFuture( DeletePatronPinResponse.respond422()));
      }
    });

  }

  public void postPatronPinVerify(Patronpin entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PostgresClient pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    String supplied_pin_derivation = getDerivation(entity.getPin(), entity.getId());

    Future<io.vertx.core.json.JsonObject> f = pgClient.getById(TABLE_NAME_PATRON_PIN, entity.getId());
    f.onComplete( res -> {
      io.vertx.core.AsyncResult ar = (io.vertx.core.AsyncResult) res;
      if (ar.succeeded()) {
        JsonObject jo = (JsonObject) ar.result();
        if ( jo.getString("pin").equals(supplied_pin_derivation) ) {
          asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond200()));
        }
        else {
          logger.info("Pins do not match");
          asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond422()));
        }
      } else {
        asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond422()));
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


