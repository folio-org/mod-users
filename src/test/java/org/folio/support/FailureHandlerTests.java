package org.folio.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;

import io.vertx.core.AsyncResult;

class FailureHandlerTests {
    final FakeHandler<AsyncResult<Response>> responseHandler = new FakeHandler<>();
    final Logger logger = mock(Logger.class);
    final Function<String, Response> responseProducer = s -> Response
      .status(500)
      .header("Content-Type", "text/plain")
      .entity(s)
      .build();
    final FailureHandler failureHandler = new FailureHandler(responseHandler,
      logger, responseProducer);

  @Test
  void shouldProduceResponseWhenHandlingFailure() {
    failureHandler.handleFailure(new Exception("some error"));

    assertThat(responseHandler.handledObject.succeeded(), is(true));

    final var response = responseHandler.handledObject.result();

    assertThat(response.getStatus(), is(500));
    assertThat(response.getHeaderString("Content-Type"), is("text/plain"));
    assertThat(response.getEntity(), is("some error"));
  }

  @Test
  void shouldLogErrorWhenHandlingFailure() {
    failureHandler.handleFailure(new Exception("some error"));

    verify(logger).error("some error");
  }
}
