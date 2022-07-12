package org.folio.support;

import static io.vertx.core.Future.succeededFuture;

import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.Logger;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

public class FailureHandler {
  public static Function<Throwable, Response> throwableToString(
    Function<String, Response> responseProducer) {

    return t -> responseProducer.apply(t.getLocalizedMessage());
  }

  private final Logger logger;
  private final Function<Throwable, Response> errorResponseMapper;

  private final Handler<AsyncResult<Response>> asyncResultHandler;

  public FailureHandler(Handler<AsyncResult<Response>> asyncResultHandler,
    Logger logger, Function<Throwable, Response> errorResponseMapper) {

    this.logger = logger;
    this.errorResponseMapper = errorResponseMapper;
    this.asyncResultHandler = asyncResultHandler;
  }

  public void handleFailure(Throwable cause) {
    logger.error(cause.getMessage());

    asyncResultHandler.handle(succeededFuture(errorResponseMapper.apply(cause)));
  }
}
