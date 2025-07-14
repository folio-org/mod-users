package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.UUID;

import javax.ws.rs.core.Response;

import org.folio.service.impl.UserRepository;
import org.folio.support.FakeHandler;
import org.folio.support.tags.UnitTest;

import org.junit.jupiter.api.Test;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;

@UnitTest
class AddressTypesApiTests {
  @Test
  void canHandleFailureWhenFetchingUsersWithAddressType() {
    var userRepository = mock(UserRepository.class);
    var handler = new FakeHandler<AsyncResult<Response>>();
    var context = mock(Context.class);

    when(userRepository.addressTypeAssignedToUser(anyString(), any()))
      .thenReturn(Future.failedFuture(new RuntimeException("my exception")));

    new AddressTypeAPI(client -> userRepository)
      .deleteAddresstypesByAddresstypeId(UUID.randomUUID().toString(),
        Map.of(), handler, context);

    assertThat(handler.handledObject.succeeded(), is(true));
    assertThat(handler.handledObject.result().getStatus(), is(500));
  }
}
