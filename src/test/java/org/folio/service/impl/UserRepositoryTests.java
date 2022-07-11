package org.folio.service.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.folio.rest.persist.PostgresClient;
import org.junit.jupiter.api.Test;

import io.vertx.core.Future;

class UserRepositoryTests {
  @Test
  void canHandleFailureWhenFetchingUsersWithAddressType() {
    var postgresClient = mock(PostgresClient.class);

    when(postgresClient.get(anyString(), any(), any(), any(), anyBoolean(), anyBoolean(), anyList()))
      .thenReturn(Future.failedFuture(new RuntimeException("my exception")));

    new UserRepository(postgresClient)
      .addressTypeAssignedToUser(UUID.randomUUID().toString());

    var future = new UserRepository(postgresClient)
      .addressTypeAssignedToUser(UUID.randomUUID().toString());

    assertThat(future.cause().getMessage(), is("my exception"));
  }
}
