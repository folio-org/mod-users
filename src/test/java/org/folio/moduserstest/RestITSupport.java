package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_MULT_CHOICE;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;

import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.Arrays;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.predicate.ResponsePredicateResult;
import junit.framework.AssertionFailedError;

import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.tools.utils.VertxUtils;

class RestITSupport {

  static final String SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  static final String SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";
  static final String HTTP_LOCALHOST = "http://localhost:";

  private static final String LOCALHOST = "localhost";

  private static Vertx vertx;
  private static Context context;
  private static WebClient client;
  private static int port;


  private RestITSupport() {
  }

  static void setUp() {
    vertx = VertxUtils.getVertxWithExceptionHandler();
    context = vertx.getOrCreateContext();
    client = WebClient.create(vertx);
    port = NetworkUtils.nextFreePort();
  }

  static Vertx vertx() {
    return vertx;
  }

  static Context context() {
    return context;
  }

  static WebClient webClient() {
    return client;
  }

  static int port() {
    return port;
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


  static Future<HttpResponse<Buffer>> post(String request, String body) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.post(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("X-Okapi-Url", RestITSupport.HTTP_LOCALHOST + port)
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .sendBuffer(Buffer.buffer(body), promise);

    return promise.future();
  }

  static Future<Void> postWithOkStatus(String userId, String request, String body) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.post(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("X-Okapi-Url", RestITSupport.HTTP_LOCALHOST + port)
      .putHeader(OKAPI_USERID_HEADER, userId)
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .expect(res ->
        res.statusCode() >= HTTP_OK && res.statusCode() < HTTP_MULT_CHOICE
              ? ResponsePredicateResult.success()
              : ResponsePredicateResult.failure("Got status code: " + res.statusCode())
      )
      .sendBuffer(Buffer.buffer(body), promise);

    return promise.future().mapEmpty();
  }

  static Future<HttpResponse<Buffer>> put(String request, String body) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.put(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("X-Okapi-Url", RestITSupport.HTTP_LOCALHOST + port)
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .sendBuffer(Buffer.buffer(body), promise);

    return promise.future();
  }

  static Future<Void> putWithNoContentStatus(TestContext context, String userId, String request, String body) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.put(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("X-Okapi-Url", RestITSupport.HTTP_LOCALHOST + port)
      .putHeader(OKAPI_USERID_HEADER, userId)
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_TEXT_DEF)
      .sendBuffer(Buffer.buffer(body), promise);

    return promise.future().map(res -> {
      assertStatus(context, res, HTTP_NO_CONTENT);
      return null;
    });
  }

  static Future<HttpResponse<Buffer>> delete(String request) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.delete(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("accept", "*/*")
      .send(promise);

    return promise.future();
  }

  static Future<Void> deleteWithNoContentStatus(TestContext context, String request) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.delete(port, LOCALHOST, request)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("accept", "*/*")
      .send(promise);

    return promise.future().map(res -> {
      assertStatus(context, res, HTTP_NO_CONTENT);
      return null;
    });
  }

  static Future<HttpResponse<Buffer>> get(String requestUrl) {
    Promise<HttpResponse<Buffer>> promise = Promise.promise();

    client.get(port, LOCALHOST, requestUrl)
      .putHeader(OKAPI_HEADER_TENANT, "diku")
      .putHeader("content-type", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .putHeader("accept", RestITSupport.SUPPORTED_CONTENT_TYPE_JSON_DEF)
      .send(promise);

    return promise.future();
  }

  static Future<JsonObject> getJson(TestContext context, String requestUrl) {
    return get(requestUrl).map(res -> {
      RestITSupport.assertStatus(context, res, HTTP_OK);
      return res.bodyAsJsonObject();
    });
  }
}
