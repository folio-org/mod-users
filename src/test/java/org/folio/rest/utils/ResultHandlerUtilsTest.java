package org.folio.rest.utils;

import static org.folio.rest.utils.ResultHandlerUtils.getAsyncResultHandler;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.folio.support.tags.UnitTest;

@UnitTest
@ExtendWith({ VertxExtension.class })
class ResultHandlerUtilsTest {

  @Test
  void getAsyncResultHandler_positive(VertxTestContext context) {
    var testPromise = Promise.<String>promise();
    Future.succeededFuture("success").onComplete(getAsyncResultHandler(testPromise));
    testPromise.future().onComplete(context.succeeding(result -> {
      assertThat(result, is("success"));
      context.completeNow();
    }));
  }

  @Test
  void getAsyncResultHandler_negative(VertxTestContext context) {
    var testPromise = Promise.<String>promise();
    Future.<String>failedFuture(new RuntimeException("error"))
      .onComplete(getAsyncResultHandler(testPromise));

    testPromise.future().onComplete(context.failing(error -> {
      assertThat(error.getMessage(), is("error"));
      assertThat(error, instanceOf(RuntimeException.class));
      context.completeNow();
    }));
  }
}
