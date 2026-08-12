package org.folio.support;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.any;
import static com.github.tomakehurst.wiremock.client.WireMock.anyUrl;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathMatching;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.jaxrs.model.ConfigurationEntry;

import com.github.tomakehurst.wiremock.WireMockServer;

import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class WireMockHelper {

  private final WireMockServer wireMockServer;

  public WireMockHelper(WireMockServer wireMockServer, String upstreamServerUrl) {
    this.wireMockServer = wireMockServer;
    proxyOtherRequestsTo(upstreamServerUrl);
  }

  public void mockConfiguration() {
    mockConfiguration(new ArrayList<>());
  }

  public void mockConfiguration(List<ConfigurationEntry> configs) {
    JsonObject mockResponseBody = new JsonObject()
      .put("configs", configs);

    wireMockServer.stubFor(get(urlPathMatching("/configurations/entries.*"))
      .atPriority(1)
      .willReturn(aResponse()
        .withStatus(200)
        .withBody(mockResponseBody.encodePrettily())));
  }

  private void proxyOtherRequestsTo(String upstreamServerUrl) {
    wireMockServer.stubFor(any(anyUrl())
      .atPriority(10)
      .willReturn(aResponse().proxiedFrom(upstreamServerUrl)));
  }
}
