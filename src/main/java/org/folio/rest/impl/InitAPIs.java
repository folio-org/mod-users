package org.folio.rest.impl;

import io.vertx.core.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.verticle.ConsortiumEventConsumersVerticle;

import static org.folio.event.KafkaConfigSingleton.getPropertyValue;

public class InitAPIs implements InitAPI {
  private final Logger logger = LogManager.getLogger(InitAPIs.class);

  @Override
  public void init(Vertx vertx, Context context, Handler<AsyncResult<Boolean>> handler) {
    logger.info("InitAPI starting...");
    try {
      deployConsumersVerticles(vertx)
        .map(true)
        .onComplete(handler)
        .onSuccess(x -> logger.info("Consumer Verticles were successfully started"))
        .onFailure(th -> logger.error("Consumer Verticles were not started", th));
    } catch (Throwable th) {
      logger.error("Error during module init", th);
      handler.handle(Future.failedFuture(th));
    }
  }

  private Future<?> deployConsumersVerticles(Vertx vertx) {
    int usersConsortiumConsumerInstancesNumber = Integer.parseInt(getPropertyValue("users.consortium.kafka.consumer.instancesNumber", "1"));

    return vertx.deployVerticle((ConsortiumEventConsumersVerticle.class.getName()),
      new DeploymentOptions()
        .setWorker(true)
        .setInstances(usersConsortiumConsumerInstancesNumber));
  }

}
