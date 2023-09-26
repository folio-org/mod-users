package org.folio.moduserstest;

import com.github.tomakehurst.wiremock.common.ConsoleNotifier;
import com.github.tomakehurst.wiremock.common.FileSource;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.extension.Parameters;
import com.github.tomakehurst.wiremock.extension.ResponseTransformer;
import com.github.tomakehurst.wiremock.http.Request;
import com.github.tomakehurst.wiremock.http.Response;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.http.ContentType;
import io.restassured.specification.RequestSpecification;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;
import net.mguenther.kafka.junit.EmbeddedKafkaCluster;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.jaxrs.model.TenantJob;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.VertxModule;
import org.folio.support.http.*;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.jupiter.api.AfterAll;
import org.testcontainers.containers.KafkaContainer;
import org.testcontainers.utility.DockerImageName;

import java.io.IOException;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

import static net.mguenther.kafka.junit.EmbeddedKafkaCluster.provisionWith;
import static net.mguenther.kafka.junit.EmbeddedKafkaClusterConfig.defaultClusterConfig;
import static org.folio.kafka.KafkaTopicNameHelper.getDefaultNameSpace;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.rest.utils.OkapiConnectionParams.OKAPI_URL_HEADER;

public abstract class AbstractRestTest {

  public static final String KAFKA_ENV = "ENV";
  public static final String TENANT_NAME = "diku";
  public static final String KAFKA_HOST = "KAFKA_HOST";
  public static final String KAFKA_PORT = "KAFKA_PORT";
  public static final String KAFKA_ENV_VALUE = "test-env";
  public static final String OKAPI_URL_ENV = "OKAPI_URL";
  protected static final int PORT = NetworkUtils.nextFreePort();
  protected static final String OKAPI_URL = "http://localhost:" + PORT;
  protected static int port;
  protected static final String HTTP_PORT = "http.port";
  protected static final String TENANT_ID = "diku";
  protected static final String TOKEN = "token";

  protected static Vertx vertx;
  protected static VertxModule module;
  protected static OkapiUrl okapiUrl;
  protected static OkapiHeaders okapiHeaders;
  protected static RequestSpecification spec;
  protected static KafkaConsumer<String, String> kafkaConsumer;
  protected static KafkaProducer<String, String> kafkaProducer;
  private static EmbeddedKafkaCluster kafkaCluster;

  @Rule
  public WireMockRule mockServer = new WireMockRule(
    WireMockConfiguration.wireMockConfig()
      .dynamicPort()
      .notifier(new ConsoleNotifier(true))
      .extensions(new RequestToResponseTransformer())
  );

  @BeforeClass
  @SneakyThrows
  public static void beforeAll(final TestContext context) {
    vertx = Vertx.vertx();
    kafkaCluster = provisionWith(defaultClusterConfig());
    kafkaCluster.start();
    String[] hostAndPort = kafkaCluster.getBrokerList().split(":");

    System.setProperty(KAFKA_HOST, hostAndPort[0]);
    System.setProperty(KAFKA_PORT, hostAndPort[1]);
    System.setProperty(KAFKA_ENV, KAFKA_ENV_VALUE);
    System.setProperty(OKAPI_URL_ENV, OKAPI_URL);
    runDatabase();
    deployVerticle(context);
  }

  @AfterClass
  public static void tearDownClass(final TestContext context) {
    Async async = context.async();
    vertx.close(context.asyncAssertSuccess(res -> {
      kafkaCluster.stop();
      async.complete();
    }));
  }

  private static void runDatabase() {
    PostgresClient.stopPostgresTester();
    PostgresClient.setPostgresTester(new PostgresTesterContainer());
  }

  private static void deployVerticle(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    String okapiUrl = "http://localhost:" + port;
    final DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject()
        .put(HTTP_PORT, port));

    TenantClient tenantClient = new TenantClient(okapiUrl, TENANT_ID, TOKEN);
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      postTenant(context, async, tenantClient);
    });
  }

  protected static void postTenant(TestContext context, Async async, TenantClient tenantClient) {
    TenantAttributes tenantAttributes = new TenantAttributes();
    tenantClient.postTenant(tenantAttributes).onComplete(res2 -> {
      if (res2.result().statusCode() == 201) {
        tenantClient.getTenantByOperationId(res2.result().bodyAsJson(TenantJob.class).getId(), 60000, context.asyncAssertSuccess(res3 -> {
          context.assertTrue(res3.bodyAsJson(TenantJob.class).getComplete());
          async.complete();
        }));
      } else {
        context.assertEquals("Failed to make post tenant. Received status code 400", res2.result().bodyAsString());
        async.complete();
      }
    });
  }

  @Before
  public void setUp(TestContext context) {
    spec = new RequestSpecBuilder()
      .setContentType(ContentType.JSON)
      .addHeader(OKAPI_URL_HEADER, "http://localhost:" + mockServer.port())
      .addHeader(OKAPI_TENANT_HEADER, TENANT_ID)
      .addHeader("Accept", "text/plain, application/json")
      .setBaseUri("http://localhost:" + port)
      .build();
  }


  public List<String> checkKafkaEventSent(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    ConsumerRecords<String, String> records = kafkaConsumer.poll(Duration.ofMillis(3000));
    return IteratorUtils.toList(records.iterator()).stream()
        .map(ConsumerRecord::value).collect(Collectors.toList());
  }

  @SneakyThrows
  public RecordMetadata sendEvent(String tenantId, String topic, String key, String value) {
    String topicName = formatToKafkaTopicName(tenantId, topic);
    ProducerRecord<String, String> record = new ProducerRecord<>(topicName, key, value);
    record.headers().add(OKAPI_TENANT_HEADER, TENANT_NAME.getBytes());
    return kafkaProducer.send(record).get();
  }

  public void commitAllMessagesInTopic(String tenant, String eventType) {
    kafkaConsumer.subscribe(Collections.singletonList(formatToKafkaTopicName(tenant, eventType)));
    kafkaConsumer.poll(Duration.ofMillis(1000));
  }

  private static String formatToKafkaTopicName(String tenant, String eventType) {
    return KafkaTopicNameHelper.formatTopicName(KAFKA_ENV_VALUE, getDefaultNameSpace(), tenant, eventType);
  }

  public static class RequestToResponseTransformer extends ResponseTransformer {

    public static final String NAME = "request-to-response-transformer";

    @Override
    public Response transform(Request request, Response response, FileSource files, Parameters parameters) {
      return Response.Builder.like(response).but().body(request.getBody()).build();
    }

    @Override
    public String getName() {
      return NAME;
    }

    @Override
    public boolean applyGlobally() {
      return false;
    }
  }
}
