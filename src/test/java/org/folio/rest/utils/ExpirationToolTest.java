package org.folio.rest.utils;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
@Timeout(value = 5, timeUnit = TimeUnit.SECONDS)
class ExpirationToolTest {
  @Test
  void expirationForNullTenant(Vertx vertx, VertxTestContext context) {
    final var postgresClient = mock(PostgresClient.class);
    final var expirationTool = new ExpirationTool((v, t) -> postgresClient);

    final var future = expirationTool.doExpirationForTenant(vertx, null);

    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause(), is(instanceOf(IllegalArgumentException.class)));
      assertThat(future.cause().getMessage(),
        is("Cannot expire users for undefined tenant"));
      context.completeNow();
    })));
  }

  @Test
  void expirationForTenantCanHandleException(Vertx vertx, VertxTestContext context) {
    final var postgresClient = mock(PostgresClient.class);
    final var expirationTool = new ExpirationTool((v, t) -> postgresClient);

    doThrow(new RuntimeException("pg"))
      .when(postgresClient).get(anyString(), any(), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), any(Handler.class));

    final var future = expirationTool.doExpirationForTenant(vertx, "someTenant");

    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause().getMessage(), is("pg"));
      context.completeNow();
    })));
  }

  @Test
  void expirationForTenantCanHandlePostgresClientFailure(Vertx vertx, VertxTestContext context) {
    final var postgresClient = mock(PostgresClient.class);
    final var expirationTool = new ExpirationTool((v, t) -> postgresClient);

    doAnswer((Answer<Void>) invocationOnMock -> provideFailedFutureToHandler(
      invocationOnMock, 6))
      .when(postgresClient).get(anyString(), any(), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), any(Handler.class));

    final var future = expirationTool.doExpirationForTenant(vertx, "someTenant");

    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause().getMessage(), is("Database shut down for holidays"));
      context.completeNow();
    })));
  }

  @Test
  void noUsersHaveExpired(Vertx vertx, VertxTestContext context) {
    final var postgresClient = mock(PostgresClient.class);
    final var expirationTool = new ExpirationTool((v, t) -> postgresClient);

    doAnswer((Answer<Void>) this::provideSuccessFutureToHandler)
      .when(postgresClient).get(anyString(), any(), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), any(Handler.class));

    final var future = expirationTool.doExpirationForTenant(vertx, "someTenant");

    future.onComplete(context.succeeding(i -> context.verify(() -> {
      assertThat(i, is(0));
      context.completeNow();
    })));
  }

  @Test
  void disableUserCanHandleNullPostgresClient(Vertx vertx) {
    final var expirationTool = new ExpirationTool((v, t) -> null);

    var future = expirationTool.disableUser(vertx, "myTenant", new User());

    assertThat(future.cause(), is(instanceOf(NullPointerException.class)));
  }

  @Test
  void disableUserCanHandlePostgresFailure(Vertx vertx) {
    final var postgresClient = mock(PostgresClient.class);
    final var expirationTool = new ExpirationTool((v, t) -> postgresClient);

    doAnswer((Answer<Void>) invocationOnMock -> provideFailedFutureToHandler(
      invocationOnMock, 3))
      .when(postgresClient).update(anyString(), any(User.class), any(), any(Handler.class));

    var future = expirationTool.disableUser(vertx, "myTenant", new User());

    assertThat(future.cause().getMessage(), is("Database shut down for holidays"));
  }

  private static Void provideFailedFutureToHandler(InvocationOnMock invocationOnMock,
    int handlerArgumentIndex) {

    provideFutureToHandler(invocationOnMock, handlerArgumentIndex,
      Future.failedFuture("Database shut down for holidays"));

    return null;
  }

  private Void provideSuccessFutureToHandler(InvocationOnMock invocationOnMock) {
    var results = new Results<User>();

    results.setResults(Collections.emptyList());

    provideFutureToHandler(invocationOnMock, 6,
      Future.succeededFuture(results));

    return null;
  }

  private static <T> void provideFutureToHandler(InvocationOnMock invocationOnMock,
    int handlerArgumentIndex, Future<T> results) {

    final Handler<AsyncResult<T>> handler = invocationOnMock.getArgument(
      handlerArgumentIndex);

    handler.handle(results);
  }
}
