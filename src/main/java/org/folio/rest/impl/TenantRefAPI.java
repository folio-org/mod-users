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

  private static final String SAMPLE_LEAD = "sample-data";
  private static final String SAMPLE_KEY = "loadSample";
  private static final String REFERENCE_KEY = "loadReference";
  private static final String REFERENCE_LEAD = "ref-data";

  private static final Logger log = LogManager.getLogger();

  @Validate
  @Override
  Future<Integer> loadData(TenantAttributes attributes, String tenantId,
                           Map<String, String> headers, Context vertxContext) {
    return super.loadData(attributes, tenantId, headers, vertxContext)
        .compose(superRecordsLoaded -> {
          log.info("loading data to tenant");
          TenantLoading tl = new TenantLoading();
          tl.withKey(REFERENCE_KEY).withLead(REFERENCE_LEAD);
          tl.withIdContent();
          tl.add("groups");
          tl.withIdContent();
          tl.add("addresstypes");
          tl.add("users");
          tl.withKey(SAMPLE_KEY).withLead(SAMPLE_LEAD);
          tl.withIdContent();
          return tl.perform(attributes, headers, vertxContext, superRecordsLoaded);
        });
  }
}
