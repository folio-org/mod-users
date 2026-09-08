package org.folio.client;

import org.folio.rest.jaxrs.model.ConfigurationEntry;

import io.vertx.core.Future;

public interface ConfigurationClient {
  Future<ConfigurationEntry> getConfiguration(String module, String configName);
}
