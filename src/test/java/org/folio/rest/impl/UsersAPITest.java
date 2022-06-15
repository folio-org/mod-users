package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.ws.rs.core.Response;

import org.folio.cql2pgjson.exception.FieldException;
import org.folio.rest.jaxrs.model.Address;
import org.folio.rest.jaxrs.model.AddressType;
import org.folio.rest.jaxrs.model.CustomFields;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.interfaces.Results;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.z3950.zing.cql.CQLParseException;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
@Timeout(value = 5, timeUnit = TimeUnit.SECONDS)
class UsersAPITest {
  String response(String message, Throwable e) {
    Response response = UsersAPI.response(message, e, /* lang */ null,
        s -> Response.ok("400 " + s).build(),
        s -> Response.ok("500 " + s).build());
    return response.getEntity().toString();
  }

  @Test
  void responseNull() {
    assertThat(response(null, null), startsWith("500 Internal Server Error"));
  }

  @Test
  void responseCQLParseException() {
    assertThat(response("m", new CQLParseException("foo", 0)), is("400 CQL Parsing Error for 'm': foo"));
  }

  @Test
  void responseFieldException() {
    assertThat(response("x", new FieldException("bar")), is("400 CQL Parsing Error for 'x': bar"));
  }

  @Test
  void responseIllegalStateException() {
    assertThat(response("y", new RuntimeException(new IllegalStateException("z"))),
        is("400 CQL Illegal State Error for 'y': z"));
  }

  @Test
  void responseException() {
    Exception e = new IllegalArgumentException() {
      @Override
      public Throwable getCause() {
        throw new IllegalArgumentException("bee");
      }
    };
    assertThat(response(null, e), startsWith("500 bee"));
  }

  @Test
  void getUsersExceptionInCatch(VertxTestContext vtc) {
    new UsersAPI().getUsers(null, null, null, 0, 0, null, null, null, null,
        vtc.succeeding(response -> vtc.verify( () -> {
          assertThat(response.getStatus(), is(500));
          vtc.completeNow();
        })), null);
  }

  @Test
  void postUsersException(VertxTestContext vtc) {
    new UsersAPI().postUsers(null, null, null, null,
        vtc.succeeding(response -> vtc.verify( () -> {
          assertThat(response.getStatus(), is(500));
          vtc.completeNow();
        })), null);
  }

  @Test
  void postUsersRemoveCustomFieldIfEmptyString(VertxTestContext vtc) {
    Map<String,String> okapiHeaders = new HashMap<>();
    CustomFields customFields = new CustomFields().withAdditionalProperty("test", "");
    User user = new User().withCustomFields(customFields);

    new UsersAPI().postUsers(null, user, null, okapiHeaders,
      vtc.succeeding(response -> vtc.verify( () -> {
        assertTrue(user.getCustomFields().getAdditionalProperties().isEmpty());
        vtc.completeNow();
      })), Vertx.vertx().getOrCreateContext());
  }

  @Test
  void postUsersExceptionInOtherwise(VertxTestContext vtc) {
    new UsersAPI().postUsers(null, new User(), null, null,
        vtc.succeeding(response -> vtc.verify( () -> {
          assertThat(response.getStatus(), is(500));
          vtc.completeNow();
        })), null);
  }

  @Test
  void postUsersExceptionInOnFailure(VertxTestContext vtc) {
    Map<String,String> okapiHeaders = new HashMap<>() {
      @Override
      public String get(Object key) {
        throw new IllegalArgumentException() {
          public String getMessage() {
            throw new IllegalArgumentException();
          }
        };
      }
    };
    new UsersAPI().postUsers(null, new User(), null, okapiHeaders,
        vtc.succeeding(response -> vtc.verify( () -> {
          assertThat(response.getStatus(), is(500));
          vtc.completeNow();
        })), null);
  }

  @Test
  void putUsersByUserIdException(VertxTestContext vtc) {
    new UsersAPI().putUsersByUserId(null, null, null, null,
        vtc.succeeding(response -> vtc.verify( () -> {
          assertThat(response.getStatus(), is(500));
          vtc.completeNow();
        })), null);
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

  private void checkAddressTypeValid(VertxTestContext context,
    AsyncResult<Results<AddressType>> result, Handler<Throwable> handler) {

    final var postgresClient = mock(PostgresClient.class);

    doAnswer((Answer<Void>) invocationOnMock -> provideFutureToHandler(
      invocationOnMock, result))
      .when(postgresClient).get(anyString(), any(), any(Criterion.class), anyBoolean(), any());

    final var future = new UsersAPI()
      .checkAddressTypeValid("someAddressTypeId", Vertx.vertx().getOrCreateContext(), postgresClient);

    future.onComplete(context.failing(e ->
      context.verify(() -> handler.handle(future.cause()))));
  }

  private static Void provideFutureToHandler(InvocationOnMock invocationOnMock,
    AsyncResult<Results<AddressType>> result) {

    final Handler<AsyncResult<Results<AddressType>>> handler = invocationOnMock.getArgument(4);

    handler.handle(result);

    return null;
  }
}

