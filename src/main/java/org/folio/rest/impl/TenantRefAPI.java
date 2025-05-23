package org.folio.rest.impl;

import java.util.Map;

import javax.ws.rs.core.Response;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dbschema.Versioned;
import org.folio.event.KafkaConfigSingleton;
import org.folio.kafka.services.KafkaAdminClientService;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.tools.utils.TenantLoading;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.support.kafka.topic.UsersKafkaTopic;

public class TenantRefAPI extends TenantAPI {

  private static final Logger log = LogManager.getLogger();

  @Validate
  @Override
  Future<Integer> loadData(TenantAttributes attributes, String tenantId,
      Map<String, String> headers, Context vertxContext) {

    return super.loadData(attributes, tenantId, headers, vertxContext)
        .compose(superRecordsLoaded -> {
          Future<Void> f = Future.succeededFuture();
          if (KafkaConfigSingleton.INSTANCE.isKafkaEnabled()) {
            log.info("creating kafka topics");
            f = createTopics(tenantId, vertxContext);
          } else {
            log.info("Kafka is not enabled, skipping topic creation");
          }
          log.info("loading data to tenant");
          TenantLoading tl = new TenantLoading();

          tl.withKey("loadReference").withLead("ref-data");
          if (isNew(attributes, "15.4.0")) {
            tl.withIdContent().add("addresstypes-15.4.0", "addresstypes");
          }
          if (isNew(attributes, "17.3.0")) {
            tl.withIdContent().add("groups-17.3.0", "groups");
          }

          tl.withKey("loadSample").withLead("sample-data");
          if (isNew(attributes, "15.4.0")) {
            tl.withIdContent().add("users-15.4.0", "users");
          }
          if (isNew(attributes, "17.3.0")) {
            tl.withIdContent().add("users-17.3.0", "users");
          }
          if (isNew(attributes, "19.4.0")) {
            tl.withIdContent().add("groups-19.4.0", "groups");
          }

          return f.compose(x -> tl.perform(attributes, headers, vertxContext, superRecordsLoaded));
        });
  }

  @Validate
  @Override
  public void postTenant(TenantAttributes tenantAttributes, Map<String, String> headers,
    Handler<AsyncResult<Response>> handler, Context context) {

    // delete Kafka topics if tenant purged
    var tenantId = TenantTool.tenantId(headers);
    Future<Void> result =
      KafkaConfigSingleton.INSTANCE.isKafkaEnabled() && Boolean.TRUE.equals(tenantAttributes.getPurge())
        ? deleteTopics(tenantId, context)
        : Future.succeededFuture();
    result.onComplete(x -> super.postTenant(tenantAttributes, headers, handler, context));
  }


  static Future<Void> createTopics(String tenantId, Context context) {
    return new KafkaAdminClientService(context.owner())
              .createKafkaTopics(UsersKafkaTopic.values(), tenantId);
  }

  static Future<Void> deleteTopics(String tenantId, Context context) {
    return new KafkaAdminClientService(context.owner())
      .deleteKafkaTopics(UsersKafkaTopic.values(), tenantId);
  }

  /**
   * Returns attributes.getModuleFrom() < featureVersion or attributes.getModuleFrom() is null.
   */
  private static boolean isNew(TenantAttributes attributes, String featureVersion) {
    if (attributes.getModuleFrom() == null) {
      return true;
    }
    var since = new Versioned() {
    };
    since.setFromModuleVersion(featureVersion);
    return since.isNewForThisInstall(attributes.getModuleFrom());
  }
}
