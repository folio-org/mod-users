package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.interfaces.Results;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
@Timeout(value = 5, timeUnit = TimeUnit.SECONDS)
public class UsersAPITest {
  private void checkAddressTypeValid(VertxTestContext context,
      AsyncResult<Results<AddressType>> result, Handler<Throwable> handler) {

    PostgresClient postgresClient = mock(PostgresClient.class);
    Future<Boolean> future = new UsersAPI()
        .checkAddressTypeValid("someAddressTypeId", Vertx.vertx().getOrCreateContext(), postgresClient);
    ArgumentCaptor<Handler<AsyncResult<Results<AddressType>>>> handlerCaptor = ArgumentCaptor.forClass(Handler.class);
    verify(postgresClient, timeout(100)).get(anyString(), any(), any(Criterion.class), anyBoolean(), handlerCaptor.capture());
    handlerCaptor.getValue().handle(result);
    future.onComplete(context.failing(e -> context.verify(() -> {
      handler.handle(future.cause());
    })));
  }

  @Test
  void checkAddressTypeValidCanHandlePostgresClientFailure(VertxTestContext context) {
    checkAddressTypeValid(context, Future.failedFuture(new RuntimeException("postgres failed")), throwable -> {
      assertThat(throwable.getMessage(), is("postgres failed"));
      context.completeNow();
    });
  }

  @Test
  void checkAddressTypeValidCanHandleException(VertxTestContext context) {
    checkAddressTypeValid(context, null, throwable -> {
      assertThat(throwable, is(instanceOf(NullPointerException.class)));
      context.completeNow();
    });
  }

  @Test
  void checkAddressTypeValidCanHandleNullPostgresClient(Vertx vertx, VertxTestContext context) {
    Future<Boolean> future = new UsersAPI().checkAddressTypeValid("myId", vertx.getOrCreateContext(),
        /* postgresClient */ null);
    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause(), is(instanceOf(NullPointerException.class)));
      context.completeNow();
    })));
  }

  @Test
  void checkAllAddressTypesValidCanHandleNullPostgresClient(Vertx vertx, VertxTestContext context) {
    Address address = new Address().withAddressTypeId("someAddressTypeId");
    Personal personal = new Personal().withAddresses(Collections.singletonList(address));
    User user = new User().withPersonal(personal);
    Future<Boolean> future = new UsersAPI().checkAllAddressTypesValid(user, vertx.getOrCreateContext(),
        /* postgresClient */ null);
    future.onComplete(context.failing(e -> context.verify(() -> {
      assertThat(future.cause(), is(instanceOf(NullPointerException.class)));
      context.completeNow();
    })));
  }
}

