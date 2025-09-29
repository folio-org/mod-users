package org.folio.moduserstest;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static java.time.Duration.ofMillis;
import static java.time.Duration.ofMinutes;
import static org.folio.extensions.KafkaContainerExtension.createTopics;
import static org.folio.extensions.KafkaContainerExtension.getTopicName;
import static org.folio.support.TestConstants.ENV;
import static org.folio.support.TestConstants.TENANT_NAME;
import static org.folio.test.util.TestUtil.readFile;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;

import io.vertx.core.Vertx;
import io.vertx.core.json.Json;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.testcontainers.shaded.org.awaitility.Awaitility;
import org.testcontainers.shaded.org.awaitility.core.ThrowingRunnable;

import org.folio.event.ConsortiumEventType;
import org.folio.extensions.KafkaContainerExtension;
import org.folio.extensions.LocalStackContainerExtension;
import org.folio.extensions.PostgresContainerExtension;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.FakeTokenGenerator;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;

import com.github.tomakehurst.wiremock.WireMockServer;

import lombok.SneakyThrows;

@ExtendWith({
  VertxExtension.class,
  KafkaContainerExtension.class,
  LocalStackContainerExtension.class,
  PostgresContainerExtension.class,
})
public abstract class AbstractRestTest {

  public static final WireMockServer wireMockServer = new WireMockServer(options().dynamicPort());

  protected static OkapiUrl okapiUrl;
  protected static VertxModule module;
  protected static OkapiHeaders okapiHeaders;

  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context, boolean hasData) {
    System.setProperty("ENV", ENV);
    wireMockServer.start();
    KafkaContainerExtension.enableKafka();
    createTopics(getConsortiumTopicNames());
    final var port = NetworkUtils.nextFreePort();
    final var token = new FakeTokenGenerator().generateToken();

    okapiUrl = new OkapiUrl("http://localhost:" + port);
    okapiHeaders = new OkapiHeaders(okapiUrl, TENANT_NAME, token);
    module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(okapiHeaders, hasData, hasData))
      .onComplete(context.succeedingThenComplete());
  }

  @AfterAll
  static void afterAll(Vertx vertx, VertxTestContext context) {
    module.purgeModule(okapiHeaders)
      .onComplete(context.succeedingThenComplete())
      .onComplete(unused -> vertx.close());
    wireMockServer.stop();
  }

  private static List<String> getConsortiumTopicNames() {
    return Arrays.stream(ConsortiumEventType.values())
      .map(ConsortiumEventType::getTopicName)
      .map(eventType -> getTopicName(TENANT_NAME, eventType))
      .toList();
  }

  protected static void awaitUntilAsserted(ThrowingRunnable runnable) {
    awaitUntilAsserted(runnable, ofMinutes(1), ofMillis(500));
  }

  protected static void awaitUntilAsserted(ThrowingRunnable runnable,
    Duration atMostDuration, Duration pollIntervalDuration) {
    Awaitility.await()
      .atMost(atMostDuration)
      .pollInterval(pollIntervalDuration)
      .untilAsserted(runnable);
  }

  protected static <T> T readObjectFromFile(String pathToJson, Class<T> clazz) {
    var jsonString = readExistedFile(pathToJson);
    assertThat(jsonString, notNullValue());
    return Json.decodeValue(jsonString, clazz);
  }

  protected static String readExistedFile(String pathToJson) {
    try {
      return readFile(pathToJson);
    } catch (IOException | URISyntaxException e) {
      Assertions.fail(e.getMessage());
      return null;
    }
  }
}
