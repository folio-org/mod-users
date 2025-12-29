package org.folio.rest.utils;

import static org.folio.rest.utils.ResultHandlerUtils.getAsyncResultHandler;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

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

  @Test
  void verifyThatUtilityClassCannotBeInstantiated() {
    var declaredConstructors = ResultHandlerUtils.class.getDeclaredConstructors();
    assertThat(declaredConstructors.length, is(1));

    var declaredConstructor = declaredConstructors[0];
    assertThat(Modifier.isPrivate(declaredConstructor.getModifiers()), is(true));
    declaredConstructor.setAccessible(true);

    var error = assertThrows(InvocationTargetException.class, declaredConstructor::newInstance);
    assertThat(error.getCause(), instanceOf(IllegalStateException.class));
    assertThat(error.getCause().getMessage(), is("Utility class cannot be instantiated"));
  }
}
