package org.folio.rest.impl;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

import org.folio.rest.jaxrs.model.ProxiesFor;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.interfaces.Results;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;

public class ProxiesForAPITest {
  @Test
  void userAndProxyUserComboExistsCanHandlePostgresClientFailure() {
    PostgresClient postgresClient = mock(PostgresClient.class);
    Future<Boolean> future = new ProxiesForAPI()
        .userAndProxyUserComboExists("someUserId", "someProxyUserId", postgresClient);
    ArgumentCaptor<Handler<AsyncResult<Results<ProxiesFor>>>> handlerCaptor = ArgumentCaptor.forClass(Handler.class);
    verify(postgresClient).get(anyString(), any(), any(Criterion.class), anyBoolean(), handlerCaptor.capture());
    handlerCaptor.getValue().handle(Future.failedFuture(new RuntimeException("my exception")));
    assertThat(future.cause().getMessage(), is("my exception"));
  }
}
