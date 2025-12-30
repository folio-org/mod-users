package org.folio.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import org.folio.event.KafkaConfigSingleton;
import org.folio.event.util.PomReaderUtil;
import org.folio.kafka.GlobalLoadSensor;
import org.folio.kafka.KafkaConfig;
import org.folio.kafka.SubscriptionDefinition;
import org.folio.kafka.KafkaTopicNameHelper;
import org.folio.kafka.KafkaConsumerWrapper;
import org.folio.kafka.AsyncRecordHandler;
import org.folio.rest.tools.utils.ModuleName;

import java.util.ArrayList;
import java.util.List;

import static org.folio.event.KafkaConfigSingleton.getPropertyValue;

public abstract class AbstractConsumersVerticle extends AbstractVerticle {

  private static final GlobalLoadSensor globalLoadSensor = new GlobalLoadSensor();

  @Override
  public void start(Promise<Void> startPromise) {
    List<Future<Void>> futures = new ArrayList<>();
    KafkaConfig kafkaConfig = KafkaConfigSingleton.INSTANCE.getKafkaConfig();
    int loadLimit = Integer.parseInt(getPropertyValue("users.kafka.UsersConsumer.loadLimit", "5"));
    getEvents().forEach(event -> {
      SubscriptionDefinition subscriptionDefinition = KafkaTopicNameHelper
        .createSubscriptionDefinition(kafkaConfig.getEnvId(),
          KafkaTopicNameHelper.getDefaultNameSpace(),
          event);
      KafkaConsumerWrapper<String, String> consumerWrapper = KafkaConsumerWrapper.<String, String>builder()
        .context(context)
        .vertx(vertx)
        .kafkaConfig(kafkaConfig)
        .loadLimit(loadLimit)
        .globalLoadSensor(globalLoadSensor)
        .subscriptionDefinition(subscriptionDefinition)
        .build();

      futures.add(consumerWrapper.start(getHandler(),
        constructModuleName() + "_" + getClass().getSimpleName()));
    });

    Future.all(futures).onComplete(ar -> startPromise.complete());
  }

  private String constructModuleName() {
    return PomReaderUtil.INSTANCE.constructModuleVersionAndVersion(ModuleName.getModuleName(),
      ModuleName.getModuleVersion());
  }

  /**
   * Events that consumer subscribed to.
   *
   * @return list of events
   */
  public abstract List<String> getEvents();

  /**
   * Handler that will be invoked when kafka messages comes to processing.
   *
   * @return handler to porcess kafka message
   */
  public abstract AsyncRecordHandler<String, String> getHandler();
}
