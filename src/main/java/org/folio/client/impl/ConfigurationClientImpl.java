package org.folio.client.impl;

import static java.util.function.Predicate.not;

import java.util.Map;
import java.util.Optional;

import org.folio.client.ConfigurationClient;
import org.folio.integration.http.ResponseInterpreter;
import org.folio.integration.http.VertxOkapiHttpClient;
import org.folio.rest.jaxrs.model.ConfigurationEntry;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class ConfigurationClientImpl implements ConfigurationClient {

  private static final String CONFIGS_ARRAY_NAME = "configs";
  private static final String QUERY = "query";
  private static final String LIMIT = "limit";
  private static final String CONFIGURATION_ENTRIES_PATH = "/configurations/entries";
  private static final String QUERY_TEMPLATE = "module==\"%s\" AND configName==\"%s\"";

  private final VertxOkapiHttpClient client;
  private final Map<String, String> headers;

  public ConfigurationClientImpl(VertxOkapiHttpClient client, Map<String, String> headers) {
    this.client = client;
    this.headers = headers;
  }

  @Override
  public Future<ConfigurationEntry> getConfiguration(String module, String configName) {
    Map<String, String> queryParams = Map.of(
      QUERY, QUERY_TEMPLATE.formatted(module, configName),
      LIMIT, "1");

    return client.get(CONFIGURATION_ENTRIES_PATH, queryParams, headers)
      .compose(ResponseInterpreter::verifyAndExtractBody)
      .map(ConfigurationClientImpl::extractConfiguration);
  }

  private static ConfigurationEntry extractConfiguration(JsonObject responseBody) {
    return Optional.ofNullable(responseBody)
      .map(body -> body.getJsonArray(CONFIGS_ARRAY_NAME))
      .filter(not(JsonArray::isEmpty))
      .map(configs -> configs.getJsonObject(0))
      .map(json -> json.mapTo(ConfigurationEntry.class))
      .orElse(null);
  }

}
