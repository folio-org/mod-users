package org.folio.rest.utils;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_NO_CONTENT;
import static org.apache.http.HttpStatus.SC_OK;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import io.vertx.core.Future;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.client.HttpResponse;

import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.support.tags.UnitTest;

@UnitTest
public class TenantInit {
  private TenantInit() {}

  public static Future<HttpResponse<Buffer>> init(TenantClient tenantClient, TenantAttributes ta) {
    return tenantClient.postTenant(ta)
        .compose(res -> {
          int responseStatusCode = res.statusCode();
          if (responseStatusCode != SC_CREATED) {
            return succeededFuture(res);
          }
          assertThat("tenant POST status", responseStatusCode, is(SC_CREATED));
          assertThat("tenant POST error", res.bodyAsJsonObject().getString("error"), is(nullValue()));
          var jsonObject = res.bodyAsJsonObject();
          return tenantClient.getTenantByOperationId(jsonObject.getString("id"), 50000)
              .compose(res2 -> {
                assertThat("tenant GET status", res2.statusCode(), is(SC_OK));
                assertThat("tenant GET error", res2.bodyAsJsonObject().getString("error"), is(nullValue()));
                return tenantClient.deleteTenantByOperationId(jsonObject.getString("id"));
              })
              .map(res3 -> {
                assertThat("tenant DELETE status", res3.statusCode(), is(SC_NO_CONTENT));
                return res3;
              });
        });
  }
}
