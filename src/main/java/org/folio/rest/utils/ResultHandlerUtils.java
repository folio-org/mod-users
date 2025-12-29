package org.folio.rest.utils;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Promise;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ResultHandlerUtils {

  /**
   * Returns a handler that completes the given promise based on the result of the asynchronous operation.
   *
   * @param promise the promise to be completed
   * @param <R> the type of the result
   * @return a handler for the asynchronous result
   */
  public static <R> Handler<AsyncResult<R>> getAsyncResultHandler(Promise<R> promise) {
    return ar -> {
      if (ar.succeeded()) {
        promise.succeed(ar.result());
      } else {
        promise.fail(ar.cause());
      }
    };
  }
}
