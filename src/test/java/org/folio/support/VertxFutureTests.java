package org.folio.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import io.vertx.core.Future;

class VertxFutureTests {
  @Test
  void exceptionsThrownDuringSuccessHandlerAreRethrownOnMainThread() {
    var future = Future.succeededFuture(2);

    final var exception = assertThrows(RuntimeException.class, () -> future
      .onSuccess(i -> {
        throw new RuntimeException("foo");
      }));

    assertThat(exception.getMessage(), is("foo"));
  }

  @Test
  void exceptionsThrownDuringFailureHandlerAreRethrownOnMainThread() {
    var future = Future.failedFuture(new RuntimeException("bar"));

    final var exception = assertThrows(RuntimeException.class, () -> future
      .onFailure(i -> {
        throw new RuntimeException("foo");
      }));

    assertThat(exception.getMessage(), is("foo"));
  }
}
