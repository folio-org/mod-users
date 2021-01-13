package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.tools.utils.TenantLoading;
import org.folio.service.impl.kafka.topic.KafkaAdminClientService;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

public class TenantRefAPI extends TenantAPI {

  private static final String SAMPLE_LEAD = "sample-data";
  private static final String SAMPLE_KEY = "loadSample";
  private static final String REFERENCE_KEY = "loadReference";
  private static final String REFERENCE_LEAD = "ref-data";

  private static final Logger log = LogManager.getLogger();
  final String[] refPaths = new String[]{
    "users",
    "groups",
    "addresstypes",
    "proxiesfor",
    "departments",
    "custom-fields"
  };

  List<JsonObject> servicePoints = null;

  String servicePointUserFilter(String s) {
    JsonObject jInput = new JsonObject(s);
    JsonObject jOutput = new JsonObject();
    jOutput.put("userId", jInput.getString("id"));
    JsonArray ar = new JsonArray();
    for (JsonObject pt : servicePoints) {
      ar.add(pt.getString("id"));
    }
    jOutput.put("servicePointsIds", ar);
    jOutput.put("defaultServicePointId", ar.getString(0));
    String res = jOutput.encodePrettily();
    log.info("servicePointUser result : " + res);
    return res;
  }

  @Override
  @Validate
  public void postTenant(TenantAttributes tenantAttributes, Map<String, String> headers,
    Handler<AsyncResult<Response>> handler, Context context) {
    
    new KafkaAdminClientService(context.owner())
      // have to create topics before tenant init,
      // because on init we can create sample/ref data which
      // has to be sent to the queue as well
      .createKafkaTopics()
      .onComplete(topicCreateResult -> {
        if (topicCreateResult.failed()) {
          log.error("Unable to create kafka topics", topicCreateResult.cause());
          handler.handle(succeededFuture(PostTenantResponse
            .respond500WithTextPlain(topicCreateResult.cause().getMessage())));
        } else {
          log.info("Topics created successfully, proceeding with tenant initialization...");
          super.postTenant(tenantAttributes, headers, handler, context);
        }
      });
  }

  @Validate
  @Override
  Future<Integer> loadData(TenantAttributes attributes, String tenantId,
                           Map<String, String> headers, Context vertxContext) {
    return super.loadData(attributes, tenantId, headers, vertxContext)
        .compose(superRecordsLoaded -> {
          try {
            List<URL> urls = TenantLoading.getURLsFromClassPathDir(
                REFERENCE_LEAD + "/service-points");
            servicePoints = new LinkedList<>();
            for (URL url : urls) {
              InputStream stream = url.openStream();
              String content = IOUtils.toString(stream, StandardCharsets.UTF_8);
              stream.close();
              servicePoints.add(new JsonObject(content));
            }
          } catch (URISyntaxException | IOException ex) {
            return Future.failedFuture(ex.getMessage());
          }
          TenantLoading tl = new TenantLoading();

          tl.withKey(REFERENCE_KEY).withLead(REFERENCE_LEAD);
          tl.withIdContent();
          for (String p : refPaths) {
            tl.add(p);
          }
          tl.withKey(REFERENCE_KEY).withLead(REFERENCE_LEAD);
          tl.withIdContent();
          tl.add("groups");
          tl.withIdContent();
          tl.add("addresstypes");
          tl.withKey(SAMPLE_KEY).withLead(SAMPLE_LEAD);
          tl.withIdContent();
          tl.add("users");
          return tl.perform(attributes, headers, vertxContext, superRecordsLoaded);
        });
  }
}
