package org.folio.moduserstest;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.web.client.HttpResponse;
import java.util.Arrays;
import junit.framework.AssertionFailedError;

class RestITSupport {

  static final String SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  static final String SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";
  static final String HTTP_LOCALHOST = "http://localhost:";

  RestITSupport() {
  }

  static void fail(TestContext context, HttpClientResponse response) {
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this fail method from the stacktrace
    fail(context, null, response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  static void fail(TestContext context, String message, HttpClientResponse response) {
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this fail method from the stacktrace
    fail(context, message, response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  static void fail(TestContext context, String message, HttpClientResponse response,
                           StackTraceElement [] stacktrace) {
    Async async = context.async();
    response.bodyHandler(body -> {
      Throwable t = new AssertionFailedError((message == null ? "" : message + ": ")
          + response.statusCode() + " " + response.statusMessage() + " " + body.toString());
      // t contains the stacktrace of bodyHandler but does not contain the method that
      // called this fail method. Therefore exchange the stacktrace:
      t.setStackTrace(stacktrace);
      context.fail(t);
      async.complete();
    });
  }

  static void fail(TestContext context, String message, HttpResponse<Buffer> response,
                           StackTraceElement [] stacktrace) {
    Async async = context.async();

    Throwable t = new AssertionFailedError((message == null ? "" : message + ": ")
      + response.statusCode() + " " + response.statusMessage() + " " + response.bodyAsString());
    // t contains the stacktrace of bodyHandler but does not contain the method that
    // called this fail method. Therefore exchange the stacktrace:
    t.setStackTrace(stacktrace);
    context.fail(t);
    async.complete();
  }

  /**
   * Fail the context if response does not have the provided status.
   */
  static void assertStatus(TestContext context, HttpClientResponse response, int status) {
    if (response.statusCode() == status) {
      return;
    }
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this assertStatus method from the stacktrace
    fail(context, "Expected status " + status + " but got",
        response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  static void assertStatus(TestContext context, HttpResponse<Buffer> response, int status) {
    if (response.statusCode() == status) {
      return;
    }
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this assertStatus method from the stacktrace
    fail(context, "Expected status " + status + " but got",
      response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

}
