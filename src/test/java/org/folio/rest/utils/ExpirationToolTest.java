package org.folio.rest.utils;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;
import org.folio.rest.testing.UtilityClassTester;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.ext.sql.UpdateResult;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
@Timeout(value = 5, timeUnit = TimeUnit.SECONDS)
public class ExpirationToolTest {
  @AfterEach
  void cleanup() {
    ExpirationTool.postgresClient = PostgresClient::getInstance;
  }

  @Test
  void isUtilityClass() {
    UtilityClassTester.assertUtilityClass(ExpirationTool.class);
  }

  @Test
  void expirationForNullTenant(Vertx vertx, VertxTestContext context) {
    Future<Integer> future = ExpirationTool.doExpirationForTenant(vertx, vertx.getOrCreateContext(), null);
    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause(), is(instanceOf(NullPointerException.class)));
      context.completeNow();
    })));
  }

  @Test
  void expirationForTenantCanHandleException(Vertx vertx, VertxTestContext context) {
    PostgresClient postgresClient = mock(PostgresClient.class);
    doThrow(new RuntimeException("pg"))
      .when(postgresClient).get(anyString(), any(Class.class), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), any());
    ExpirationTool.postgresClient = (v,t) -> postgresClient;
    Future<Integer> future = ExpirationTool.doExpirationForTenant(vertx, vertx.getOrCreateContext(), "someTenant");
    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause().getMessage(), is("pg"));
      context.completeNow();
    })));
  }

  @Test
  void expirationForTenantCanHandlePostgresClientFailure(Vertx vertx, VertxTestContext context) {
    PostgresClient postgresClient = mock(PostgresClient.class);
    ExpirationTool.postgresClient = (v,t) -> postgresClient;
    Future<Integer> future = ExpirationTool.doExpirationForTenant(vertx, vertx.getOrCreateContext(), "someTenant");
    ArgumentCaptor<Handler<AsyncResult<Results<User>>>> handlerCaptor = ArgumentCaptor.forClass(Handler.class);
    verify(postgresClient)
      .get(anyString(), any(), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), handlerCaptor.capture());
    handlerCaptor.getValue().handle(Future.failedFuture("Database shut down for holidays"));
    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause().getMessage(), is("Database shut down for holidays"));
      context.completeNow();
    })));
  }

  @Test
  void noUsersHaveExpired(Vertx vertx, VertxTestContext context) {
    PostgresClient postgresClient = mock(PostgresClient.class);
    ExpirationTool.postgresClient = (v,t) -> postgresClient;
    Future<Integer> future = ExpirationTool.doExpirationForTenant(vertx, vertx.getOrCreateContext(), "someTenant");
    ArgumentCaptor<Handler<AsyncResult<Results<User>>>> handlerCaptor = ArgumentCaptor.forClass(Handler.class);
    verify(postgresClient)
      .get(anyString(), any(), any(), any(CQLWrapper.class), anyBoolean(), anyBoolean(), handlerCaptor.capture());
    Results<User> results = new Results<>();
    results.setResults(Collections.emptyList());
    handlerCaptor.getValue().handle(Future.succeededFuture(results));
    future.onComplete(context.succeeding(i -> context.verify(() -> {
      assertThat(i, is(0));
      context.completeNow();
    })));
  }

  @Test
  void saveUserCanHandleNullPostgresClient(Vertx vertx) {
    ExpirationTool.postgresClient = (v,t) -> null;
    Future<Void> future = ExpirationTool.saveUser(vertx, "myTenant", new User());
    assertThat(future.cause(), is(instanceOf(NullPointerException.class)));
  }

  @Test
  void saveUserCanHandlePostgresFailure(Vertx vertx) {
    PostgresClient postgresClient = mock(PostgresClient.class);
    ExpirationTool.postgresClient = (v,t) -> postgresClient;
    Future<Void> future = ExpirationTool.saveUser(vertx, "myTenant", new User());
    ArgumentCaptor<Handler<AsyncResult<UpdateResult>>> handlerCaptor = ArgumentCaptor.forClass(Handler.class);
    verify(postgresClient).update(anyString(), any(User.class), any(), handlerCaptor.capture());
    handlerCaptor.getValue().handle(Future.failedFuture("out of punchcards"));
    assertThat(future.cause().getMessage(), is("out of punchcards"));
  }
}
