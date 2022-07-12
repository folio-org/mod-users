package org.folio.support;

import static io.vertx.core.Future.succeededFuture;

import javax.ws.rs.core.Response;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

public class SuccessHandler<T> {
  private final Handler<AsyncResult<Response>> asyncResultHandler;
  private final FailureHandler failureHandler;
  private final ThrowingMapper<T, Response, Exception> responseMapper;

  public SuccessHandler(Handler<AsyncResult<Response>> asyncResultHandler,
    FailureHandler failureHandler,
    ThrowingMapper<T, Response, Exception> responseMapper) {

    this.asyncResultHandler = asyncResultHandler;
    this.failureHandler = failureHandler;
    this.responseMapper = responseMapper;
  }

  public void handleSuccess(T result) {
    try {
      asyncResultHandler.handle(succeededFuture(responseMapper.map(result)));
    } catch (Exception e) {
      failureHandler.handleFailure(e);
    }
  }
}
