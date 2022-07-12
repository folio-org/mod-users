package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Patronpin;
import org.folio.rest.jaxrs.resource.PatronPin;
import org.folio.rest.persist.PgUtil;
import org.folio.service.impl.PasswordHashService;
import org.folio.service.impl.PatronPinService;
import org.folio.service.impl.PatronPinRepository;
import org.folio.support.FailureHandler;
import org.folio.support.SuccessHandler;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;


public class PatronPinAPI implements PatronPin {
  private static final Logger logger = LogManager.getLogger(PatronPinAPI.class);
  public static final String TABLE_NAME_PATRON_PIN = "patronpin";

  public void postPatronPin(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostPatronPinResponse::respond500WithTextPlain);

    final var successHandler = new SuccessHandler<String>(asyncResultHandler,
      failureHandler, s -> PostPatronPinResponse.respond201());

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    final var patronPinRepository = new PatronPinRepository(pgClient);

    Future.succeededFuture(derivePin(entity))
      .flatMap(patronPinRepository::savePin)
      .onSuccess(successHandler::handleSuccess)
      .onFailure(failureHandler::handleFailure);
  }

  private Patronpin derivePin(Patronpin patronPin) {
    final var patronPinService = new PatronPinService(new PasswordHashService());

    final var derivedPin = patronPinService.derivePin(patronPin.getPin(),
      patronPin.getId());

    patronPin.setPin(derivedPin);

    return patronPin;
  }

  public void deletePatronPin(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      DeletePatronPinResponse::respond500WithTextPlain);

    final var successHandler = new SuccessHandler<RowSet<Row>>(asyncResultHandler,
      failureHandler, s -> DeletePatronPinResponse.respond200());

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    pgClient.delete(TABLE_NAME_PATRON_PIN, entity.getId())
      .onSuccess(successHandler::handleSuccess)
      .onFailure(failureHandler::handleFailure);
  }

  public void postPatronPinVerify(Patronpin entity, Map<String, String> okapiHeaders,
    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final var failureHandler = new FailureHandler(asyncResultHandler, logger,
      PostPatronPinVerifyResponse::respond500WithTextPlain);

    final var patronPinService = new PatronPinService(new PasswordHashService());

    final var suppliedPinDerivation = patronPinService.derivePin(entity.getPin(),
      entity.getId());

    final var successHandler = new SuccessHandler<JsonObject>(asyncResultHandler,
      failureHandler, assignedPin -> {
        if (assignedPin == null) {
          logger.info("No pin assigned to {}", entity.getId());
          return PostPatronPinVerifyResponse.respond422();
        }
        else if (assignedPin.getString("pin").equals(suppliedPinDerivation)) {
            return PostPatronPinVerifyResponse.respond200();
        } else {
          logger.info("Pins do not match");
          return PostPatronPinVerifyResponse.respond422();
        }
    });

    final var pgClient = PgUtil.postgresClient(vertxContext, okapiHeaders);

    pgClient.getById(TABLE_NAME_PATRON_PIN, entity.getId())
      .onSuccess(successHandler::handleSuccess)
      .onFailure(failureHandler::handleFailure);
  }
}
