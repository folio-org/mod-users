package org.folio.rest.impl;

import java.util.Map;

import io.vertx.core.Context;
import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dbschema.Versioned;
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

          return tl.perform(attributes, headers, vertxContext, superRecordsLoaded);
        });
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
