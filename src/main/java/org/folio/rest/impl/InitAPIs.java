package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.ThreadingModel;
import io.vertx.core.Vertx;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.verticle.ConsortiumCreateEventConsumersVerticle;
import org.folio.verticle.ConsortiumDeleteEventConsumersVerticle;
import org.folio.verticle.ConsortiumUpdateEventConsumersVerticle;

import java.util.Arrays;

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
    if (!org.folio.event.KafkaConfigSingleton.INSTANCE.isKafkaEnabled()) {
      return Future.succeededFuture();
    }

    int usersConsortiumConsumerInstancesNumber = Integer.parseInt(
      getPropertyValue("users.consortium.kafka.consumer.instancesNumber", "1"));

    var consortiumCreateEventConsumer = vertx.deployVerticle(
      ConsortiumCreateEventConsumersVerticle.class.getName(),
      new DeploymentOptions()
        .setThreadingModel(ThreadingModel.WORKER)
        .setInstances(usersConsortiumConsumerInstancesNumber));

    var consortiumUpdateEventConsumer = vertx.deployVerticle(
      ConsortiumUpdateEventConsumersVerticle.class.getName(),
      new DeploymentOptions()
        .setThreadingModel(ThreadingModel.WORKER)
        .setInstances(usersConsortiumConsumerInstancesNumber));

    var consortiumDeleteEventConsumer = vertx.deployVerticle(
      ConsortiumDeleteEventConsumersVerticle.class.getName(),
      new DeploymentOptions()
        .setThreadingModel(ThreadingModel.WORKER)
        .setInstances(usersConsortiumConsumerInstancesNumber));

    return Future.all(Arrays.asList(
      consortiumCreateEventConsumer,
      consortiumDeleteEventConsumer,
      consortiumUpdateEventConsumer
    ));
  }

}
