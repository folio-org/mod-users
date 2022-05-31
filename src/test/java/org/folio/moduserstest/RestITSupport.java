package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.Arrays;

import org.folio.rest.tools.utils.VertxUtils;

import io.restassured.http.Header;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.web.client.HttpRequest;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import junit.framework.AssertionFailedError;

/**
 * For new tests consider using RestAssured instead of legacy RestITSupport.
 */
class RestITSupport {
  private static final String SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";
  private static final String HTTP_LOCALHOST = "http://localhost:";
  private static final String LOCALHOST = "localhost";
  private static WebClient client;
  private static int port;

  private RestITSupport() { }

  static void setUp(int verticlePort) {
    Vertx vertx = VertxUtils.getVertxWithExceptionHandler();
    client = WebClient.create(vertx);
    port = verticlePort;
  }

  private static void fail(TestContext context, String message, HttpResponse<Buffer> response,
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

  static void assertStatus(TestContext context, HttpResponse<Buffer> response) {
    if (response.statusCode() == HTTP_NO_CONTENT) {
      return;
    }
    StackTraceElement [] stacktrace = new Throwable().getStackTrace();
    // remove the element with this assertStatus method from the stacktrace
    fail(context, "Expected status " + HTTP_NO_CONTENT + " but got",
      response, Arrays.copyOfRange(stacktrace, 1, stacktrace.length));
  }

  static Future<Void> putWithNoContentStatus(TestContext context, String userId, String request, String body, Header ...headers) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    HttpRequest<Buffer> req = client.put(port, LOCALHOST, request);
      req.putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("X-Okapi-Url", RestITSupport.HTTP_LOCALHOST + port)
      .putHeader(OKAPI_USERID_HEADER, userId)
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF);

      Arrays.stream(headers).forEach(h->req.putHeader(h.getName(), h.getValue()));

      req.sendBuffer(Buffer.buffer(body), promise);

    return promise.future().map(res -> {
      assertStatus(context, res);
      return null;
    });
  }

  static Future<Void> deleteWithNoContentStatus(TestContext context, String request) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.delete(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("accept", "*/*")
      .send(promise);

    return promise.future().map(res -> {
      assertStatus(context, res);
      return null;
    });
  }

}
