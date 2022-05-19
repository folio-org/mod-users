package org.folio.rest.impl;

import java.util.Map;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Patronpin;
import org.folio.rest.jaxrs.resource.PatronPin;
import org.folio.rest.persist.PgUtil;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;


public class PatronPinAPI implements PatronPin {
  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);
  public static final String TABLE_NAME_PATRON_PIN = "patronpin";

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var derivedKey = getDerivation(entity.getPin(), entity.getId());
    entity.setPin(derivedKey);

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    pgClient.save(TABLE_NAME_PATRON_PIN, entity.getId(), entity, false, true)
      .onSuccess(res -> asyncResultHandler.handle(Future.succeededFuture(
        PostPatronPinResponse.respond201())))
      .onFailure(cause -> asyncResultHandler.handle(Future.succeededFuture(
        PostPatronPinResponse.respond500WithTextPlain(cause))));
  }

  public void deletePatronPin(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    pgClient.delete(TABLE_NAME_PATRON_PIN, entity.getId())
      .onSuccess(res -> asyncResultHandler.handle(Future.succeededFuture(
        DeletePatronPinResponse.respond200())))
      .onFailure(cause -> asyncResultHandler.handle(Future.succeededFuture(
        DeletePatronPinResponse.respond422())));
  }

  public void postPatronPinVerify(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    final var suppliedPinDerivation = getDerivation(entity.getPin(),
      entity.getId());

    pgClient.getById(TABLE_NAME_PATRON_PIN, entity.getId())
      .onSuccess(assignedPin -> {
        try {
          if (assignedPin == null) {
            logger.info("No pin assigned to {}", entity.getId());
            asyncResultHandler.handle(Future.succeededFuture(PostPatronPinVerifyResponse.respond422()));
          }
          else if (assignedPin.getString("pin").equals(suppliedPinDerivation)) {
            asyncResultHandler.handle(Future.succeededFuture(
              PostPatronPinVerifyResponse.respond200()));
          } else {
            logger.info("Pins do not match");
            asyncResultHandler.handle(Future.succeededFuture(
              PostPatronPinVerifyResponse.respond422()));
          }
        }
        catch (Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(
            PostPatronPinVerifyResponse.respond500WithTextPlain(e.toString())));
        }
      })
      .onFailure(cause -> asyncResultHandler.handle(Future.succeededFuture(
        PostPatronPinVerifyResponse.respond500WithTextPlain(cause.toString()))));
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
