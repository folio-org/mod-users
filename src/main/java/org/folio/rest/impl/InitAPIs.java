package org.folio.rest.impl;

import io.vertx.core.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.verticle.ConsortiumEventConsumersVerticle;

import java.util.Collections;

import static org.folio.event.KafkaConfigSingleton.getPropertyValue;

public class InitAPIs implements InitAPI {
  private final Logger logger = LogManager.getLogger(InitAPIs.class);

  @Override
  public void init(Vertx vertx, Context context, Handler<AsyncResult<Boolean>> handler) {
    logger.info("InitAPI starting...");
    try {
      deployConsumersVerticles(vertx)
        .onSuccess(car -> {
          handler.handle(Future.succeededFuture());
          logger.info("Consumer Verticles were successfully started");
        })
        .onFailure(th -> {
          handler.handle(Future.failedFuture(th));
          logger.error("Consumer Verticles were not started", th);
        });
    } catch (Throwable th) {
      logger.error("Error during module init", th);
      handler.handle(Future.failedFuture(th));
    }
  }

  private Future<?> deployConsumersVerticles(Vertx vertx) {
    Promise<String> consortiaEventsConsumer = Promise.promise();
    int usersConsortiumConsumerInstancesNumber = Integer.parseInt(getPropertyValue("users.consortium.kafka.consumer.instancesNumber", "1"));

    vertx.deployVerticle((ConsortiumEventConsumersVerticle.class.getName()),
      new DeploymentOptions()
        .setWorker(true)
        .setInstances(usersConsortiumConsumerInstancesNumber), consortiaEventsConsumer);

    return GenericCompositeFuture.all(Collections.singletonList(
      consortiaEventsConsumer.future()));
  }

}
