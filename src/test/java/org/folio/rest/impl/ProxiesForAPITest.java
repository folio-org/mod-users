package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.interfaces.Results;
import org.jetbrains.annotations.Nullable;
import org.junit.jupiter.api.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;

class ProxiesForAPITest {
  @Test
  void userAndProxyUserComboExistsCanHandlePostgresClientFailure() {
    var postgresClient = mock(PostgresClient.class);

    doAnswer((Answer<Void>) ProxiesForAPITest::provideFailedFutureToHandler)
    .when(postgresClient).get(anyString(), any(), any(Criterion.class), anyBoolean(), any());

    var future = new ProxiesForAPI()
      .userAndProxyUserComboExists("someUserId", "someProxyUserId", postgresClient);

    assertThat(future.cause().getMessage(), is("my exception"));
  }

  @Nullable
  private static Void provideFailedFutureToHandler(InvocationOnMock invocationOnMock) {
    final Handler<AsyncResult<Results<ProxiesFor>>> handler = invocationOnMock.getArgument(4);

    handler.handle(Future.failedFuture(new RuntimeException("my exception")));

    return null;
  }
}
