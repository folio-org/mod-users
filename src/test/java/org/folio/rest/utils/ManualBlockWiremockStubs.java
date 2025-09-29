package org.folio.rest.utils;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.delete;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.matching;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathMatching;

import java.util.HashMap;
import java.util.Map;

import com.github.tomakehurst.wiremock.WireMockServer;

public class ManualBlockWiremockStubs {

  public static Map<String, String> addManualBlockStubForDeleteUserById(WireMockServer wireMockServer) {
    // Mock the manual blocks get endpoint
    wireMockServer.stubFor(get(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching("\\(userId==\".*\"\\)"))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withBody("""
              {
                  "manualblocks": [
                      {
                          "id": "93d4eae6-7049-47f0-bf8f-297a3c75a357"
                      }
                  ],
                  "totalRecords": 1
              }
              """)));

    // Mock the delete manual block endpoint
    wireMockServer.stubFor(delete(urlPathMatching("/manualblocks/.*"))
      .willReturn(aResponse()
        .withStatus(204)));
    // Create custom Okapi headers with WireMock base URL for the delete operation
    Map<String, String> customHeaders = new HashMap<>();
    customHeaders.put("X-Okapi-Url",  "http://localhost:" + wireMockServer.port());
    return customHeaders;
  }

  public static Map<String, String> blankManualBlockByCQLStubForDeleteUserById1(WireMockServer wireMockServer) {
    // Mock the manual blocks get endpoint
    wireMockServer.stubFor(get(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching("\\(userId==\".*\"\\)"))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withBody("")));

    // Create custom Okapi headers with WireMock base URL for the delete operation
    Map<String, String> customHeaders = new HashMap<>();
    customHeaders.put("X-Okapi-Url",  "http://localhost:" + wireMockServer.port());
    return customHeaders;
  }

  public static Map<String, String> manualBlockByCQLStubForDeleteUserById500Error(WireMockServer wireMockServer) {
    // Mock the manual blocks get endpoint
    wireMockServer.stubFor(get(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching("\\(userId==\".*\"\\)"))
      .willReturn(aResponse()
        .withStatus(500)
        .withHeader("Content-Type", "application/json")
        .withBody("Simulated error")));

    // Mock the delete manual block endpoint
    wireMockServer.stubFor(delete(urlPathMatching("/manualblocks/.*"))
      .willReturn(aResponse()
        .withStatus(204)));
    // Create custom Okapi headers with WireMock base URL for the delete operation
    Map<String, String> customHeaders = new HashMap<>();
    customHeaders.put("X-Okapi-Url",  "http://localhost:" + wireMockServer.port());
    return customHeaders;
  }

  public static Map<String, String> deleteManualBlockByIdStub500Error(WireMockServer wireMockServer) {
    // Mock the manual blocks get endpoint
    wireMockServer.stubFor(get(urlPathMatching("/manualblocks"))
      .withQueryParam("query", matching("\\(userId==\".*\"\\)"))
      .willReturn(aResponse()
        .withStatus(200)
        .withHeader("Content-Type", "application/json")
        .withBody("""
              {
                  "manualblocks": [
                      {
                          "id": "93d4eae6-7049-47f0-bf8f-297a3c75a357"
                      }
                  ],
                  "totalRecords": 1
              }
              """)));

    // Mock the delete manual block endpoint
    wireMockServer.stubFor(delete(urlPathMatching("/manualblocks/.*"))
      .willReturn(aResponse()
        .withStatus(500)));
    // Create custom Okapi headers with WireMock base URL for the delete operation
    Map<String, String> customHeaders = new HashMap<>();
    customHeaders.put("X-Okapi-Url",  "http://localhost:" + wireMockServer.port());
    return customHeaders;
  }
}
