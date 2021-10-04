package org.folio.rest.impl;

import java.util.Map;

import io.vertx.core.Context;
import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.tools.utils.TenantLoading;

public class TenantRefAPI extends TenantAPI {

  private static final Logger log = LogManager.getLogger();

  @Validate
  @Override
  Future<Integer> loadData(TenantAttributes attributes, String tenantId,
      Map<String, String> headers, Context vertxContext) {

    return super.loadData(attributes, tenantId, headers, vertxContext)
        .compose(superRecordsLoaded -> {
          log.info("loading data to tenant");

          TenantLoading tl = new TenantLoading();

          tl.withKey("loadReference").withLead("ref-data");
          tl.withIdContent();
          tl.add("groups");
          tl.withIdContent();
          tl.add("addresstypes");

          tl.withKey("loadSample").withLead("sample-data");
          tl.withIdContent();
          tl.add("users");

          return tl.perform(attributes, headers, vertxContext, superRecordsLoaded);
        });
  }
}
