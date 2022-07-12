package org.folio.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;

import io.vertx.core.AsyncResult;

class SuccessHandlerTests {
    final FakeHandler<AsyncResult<Response>> responseHandler = new FakeHandler<>();
    final Logger logger = mock(Logger.class);
    final Function<String, Response> failureResponseMapper = s -> Response
      .status(500)
      .header("Content-Type", "text/plain")
      .entity(s)
      .build();

    final FailureHandler failureHandler = new FailureHandler(responseHandler,
      logger, failureResponseMapper);

  @Test
  void shouldMapResultToResponse() {
    final ThrowingMapper<String, Response, Exception> successResponseProducer = s ->
      Response.status(201)
        .header("Content-Type", "text/plain")
        .entity(s)
        .build();

    final SuccessHandler<String> successHandler = new SuccessHandler<>(
      responseHandler, failureHandler, successResponseProducer);

    successHandler.handleSuccess("foo");

    assertThat(responseHandler.handledObject.succeeded(), is(true));

    final var response = responseHandler.handledObject.result();

    assertThat(response.getStatus(), is(201));
    assertThat(response.getHeaderString("Content-Type"), is("text/plain"));
    assertThat(response.getEntity(), is("foo"));
  }

  @Test
  void shouldUseFailureHandlerWhenSuccessProducerFails() {
    final ThrowingMapper<String, Response, Exception> successResponseProducer
      = s -> { throw new RuntimeException("some error"); };

    final SuccessHandler<String> successHandler = new SuccessHandler<>(
      responseHandler, failureHandler, successResponseProducer);

    successHandler.handleSuccess("foo");

    assertThat(responseHandler.handledObject.succeeded(), is(true));

    final var response = responseHandler.handledObject.result();

    assertThat(response.getStatus(), is(500));
    assertThat(response.getHeaderString("Content-Type"), is("text/plain"));
    assertThat(response.getEntity(), is("some error"));
  }
}
