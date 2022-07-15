package org.folio.support;

import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.Logger;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class FailureHandler {
  private final Logger logger;
  private final Function<String, Response> errorResponseProducer;
  private final Handler<AsyncResult<Response>> asyncResultHandler;

  public FailureHandler(Handler<AsyncResult<Response>> asyncResultHandler,
    Logger logger, Function<String, Response> errorResponseProducer) {

    this.logger = logger;
    this.errorResponseProducer = errorResponseProducer;
    this.asyncResultHandler = asyncResultHandler;
  }

  public void handleFailure(Throwable cause) {
    logger.error(cause.getLocalizedMessage());

    asyncResultHandler.handle(Future.succeededFuture(
      errorResponseProducer.apply(cause.getLocalizedMessage())));
  }
}
