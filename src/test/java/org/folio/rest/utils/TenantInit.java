package org.folio.rest.utils;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.TenantAttributes;

public class TenantInit {
  private TenantInit() {}

  public static Future<Void> init(TenantClient tenantClient, TenantAttributes ta) {
    Promise<Void> promise = Promise.promise();
    try {
      tenantClient.postTenant(ta, res1 -> {
        if (res1.failed()) {
          promise.fail(res1.cause());
          return;
        }
        if (res1.result().statusCode() == 204) {
          promise.complete();
          return;
        }
        if (res1.result().statusCode() != 201) {
          promise.fail("tenant POST returned " + res1.result().statusCode());
          return;
        }
        JsonObject jsonObject = res1.result().bodyAsJsonObject();
        tenantClient.getTenantByOperationId(jsonObject.getString("id"), 50000, res2 -> {
          if (res2.failed()) {
            promise.fail(res2.cause());
            return;
          }
          if (res2.result().statusCode() != 200) {
            promise.fail("tenant GET returned " + res2.result().statusCode());
            return;
          }
          tenantClient.deleteTenantByOperationId(jsonObject.getString("id"), res3 -> {
            if (res3.failed()) {
              promise.fail(res3.cause());
              return;
            }
            if (res3.result().statusCode() != 204) {
              promise.fail("tenant DELETE returned " + res3.result().statusCode());
              return;
            }
            promise.complete();
          });
        });
      });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }
}
