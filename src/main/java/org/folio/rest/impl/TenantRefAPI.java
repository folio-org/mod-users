package org.folio.rest.impl;
import java.util.Map;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.TenantAttributes;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.folio.rest.tools.utils.TenantLoading;

public class TenantRefAPI extends TenantAPI {
 private static final Logger log = LogManager.getLogger(TenantRefAPI.class);

  @Override
  public void postTenant(TenantAttributes ta, Map<String, String> headers,
    Handler<AsyncResult<Response>> hndlr, Context cntxt) {
    log.info("postTenant");
    Vertx vertx = cntxt.owner();
    super.postTenant(ta, headers, res -> {
      if (res.failed()) {
        hndlr.handle(res);
        return;
      }
      log.info("trying to load tenant");
      TenantLoading tl = new TenantLoading();
      tl.withPostOnly()
        .withKey("loadReference").withLead("ref-data")
        .withIdContent()
        .add("groups")
        .withIdContent()
        .add("addresstypes")
        .withKey("loadSample").withLead("sample-data")
        .withIdContent()
        
        .withAcceptStatus(422) // and 422 for POST on existing item (ignored)
        .add("users")
        .perform(ta, headers, vertx, res1 -> {
          if (res1.failed()) {
            log.info("cause:" + res1.cause().getLocalizedMessage());
            hndlr.handle(io.vertx.core.Future.succeededFuture(PostTenantResponse
              .respond500WithTextPlain(res1.cause().getLocalizedMessage())));
            return;
          }
          hndlr.handle(io.vertx.core.Future.succeededFuture(PostTenantResponse
            .respond204()));
        });
    }, cntxt);
  }

}