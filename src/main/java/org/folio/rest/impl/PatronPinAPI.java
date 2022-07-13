package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Patronpin;
import org.folio.rest.jaxrs.resource.PatronPin;
import org.folio.rest.persist.PgUtil;
import org.folio.service.impl.PatronPinService;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;


public class PatronPinAPI implements PatronPin {
  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);
  public static final String TABLE_NAME_PATRON_PIN = "patronpin";

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var patronPinService = new PatronPinService();

    final var derivedPin = patronPinService.derivePin(entity.getPin(), entity.getId());

    entity.setPin(derivedPin);

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

    final var patronPinService = new PatronPinService();

    final var suppliedPinDerivation = patronPinService.derivePin(entity.getPin(),
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
}
