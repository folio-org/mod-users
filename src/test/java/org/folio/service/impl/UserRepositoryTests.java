package org.folio.service.impl;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.folio.cql2pgjson.exception.CQL2PgJSONException;
import org.folio.rest.impl.UsersAPI;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.support.tags.UnitTest;

import org.junit.jupiter.api.Test;

import io.vertx.core.Future;

@UnitTest
class UserRepositoryTests {
  @Test
  void canHandleFailureWhenFetchingUsersWithAddressType() {
    var postgresClient = mock(PostgresClient.class);

    when(postgresClient.get(anyString(), any(), any(), any(), anyBoolean(), anyBoolean(), anyList()))
      .thenReturn(Future.failedFuture(new RuntimeException("my exception")));

    new UserRepository(postgresClient)
      .addressTypeAssignedToUser(UUID.randomUUID().toString(),
        (cql, limit, offset) -> new CQLWrapper());

    var future = new UserRepository(postgresClient)
      .addressTypeAssignedToUser(UUID.randomUUID().toString(), UsersAPI::getCQL);

    assertThat(future.cause().getMessage(), is("my exception"));
  }

  @Test
  void canHandleExceptionWhenGeneratingUsersWithAddressTypeCql() {
    var postgresClient = mock(PostgresClient.class);

    var future = new UserRepository(postgresClient)
      .addressTypeAssignedToUser(UUID.randomUUID().toString(),
        (cql, limit, offset) -> { throw new CQL2PgJSONException("error"); });

    assertThat(future.cause(), instanceOf(CQL2PgJSONException.class));
    assertThat(future.cause().getMessage(), is("error"));
  }
}
