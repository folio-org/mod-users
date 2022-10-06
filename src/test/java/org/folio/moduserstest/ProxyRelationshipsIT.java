package org.folio.moduserstest;

import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.notNullValue;

import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.support.ProxyRelationship;
import org.folio.support.ValidationErrors;
import org.folio.support.VertxModule;
import org.folio.support.http.OkapiHeaders;
import org.folio.support.http.OkapiUrl;
import org.folio.support.http.ProxiesClient;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

/**
 * At the moment, proxy relationships do not require the user to exist
 * this means that these tests do not currently create users for any of the
 * users involved.
 * This is NOT how the system is intended to be used
 */
@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class ProxyRelationshipsIT {
  private static ProxiesClient proxiesClient;

  @BeforeAll
  @SneakyThrows
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    int port = NetworkUtils.nextFreePort();

    final var okapiUrl = new OkapiUrl( "http://localhost:" + port);
    final var tenant = "proxyrelationshipsit";
    final var headers = new OkapiHeaders(okapiUrl, tenant, "token");

    proxiesClient = new ProxiesClient(okapiUrl, headers);

    final var module = new VertxModule(vertx);

    module.deployModule(port)
      .compose(res -> module.enableModule(headers))
      .onComplete(context.succeedingThenComplete());
  }

  @BeforeEach
  public void beforeEach() {
    proxiesClient.deleteAllProxies();
  }

  @Test
  void userMayNominateAProxy() {
    final var userId = generateId();
    final var proxyUserId = generateId();

    final var relationship = proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(proxyUserId)
        .build());

    assertThat(relationship.getId(), is(notNullValue()));
    assertThat(relationship.getUserId(), is(userId));
    assertThat(relationship.getProxyUserId(), is(proxyUserId));
  }

  @Test
  void canGetProxyRelationship() {
    final var userId = generateId();
    final var proxyUserId = generateId();

    final var createdRelationship = proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(proxyUserId)
        .build());

    final var fetchedRelationship = proxiesClient.getProxyRelationship(
      createdRelationship.getId());

    assertThat(fetchedRelationship.getId(), is(createdRelationship.getId()));
    assertThat(fetchedRelationship.getUserId(), is(userId));
    assertThat(fetchedRelationship.getProxyUserId(), is(proxyUserId));
  }

  @Test
  void userMayHaveMultipleProxies() {
    final var userId = generateId();
    final var firstProxyUserId = generateId();
    final var secondProxyUserId = generateId();

    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(firstProxyUserId)
        .build());

    proxiesClient.attemptToCreateProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(secondProxyUserId)
        .build())
      .statusCode(is(HTTP_CREATED));
  }

  @Test
  void sameUserMayProxyForMultipleUsers() {
    final var proxyUserId = generateId();
    final var firstUserId = generateId();
    final var secondUserId = generateId();

    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(firstUserId)
        .proxyUserId(proxyUserId)
        .build());

    proxiesClient.attemptToCreateProxyRelationship(
        ProxyRelationship.builder()
          .userId(secondUserId)
          .proxyUserId(proxyUserId)
          .build())
      .statusCode(is(HTTP_CREATED));
  }

  @Test
  void userCannotProxyForTheSameUserMoreThanOnce() {
    final var userId = generateId();
    final var proxyUserId = generateId();

    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(proxyUserId)
        .build());

    final var errors = proxiesClient.attemptToCreateProxyRelationship(
        ProxyRelationship.builder()
          .userId(userId)
          .proxyUserId(proxyUserId)
          .build())
      .statusCode(is(422))
      .extract().as(ValidationErrors.class);

    assertThat(errors.getErrors().get(0).getMessage(),
      is("Proxy relationship already exists"));
  }

  @Test
  void canUpdateProxyRelationship() {
    final var createdRelationship = proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(generateId())
        .proxyUserId(generateId())
        .build());

    final var changedProxyUserId = generateId();

    proxiesClient.attemptToUpdateProxyRelationship(ProxyRelationship.builder()
      .id(createdRelationship.getId())
      .userId(generateId())
      .proxyUserId(changedProxyUserId)
      .build())
      .statusCode(is(HTTP_NO_CONTENT));

    final var fetchedRelationship = proxiesClient.getProxyRelationship(
      createdRelationship.getId());

    assertThat(fetchedRelationship.getProxyUserId(), is(changedProxyUserId));
  }

  @Test
  void cannotUpdateUnknownProxyRelationship() {
    // Create a relationship to ensure it isn't accidentally deleted
    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(generateId())
        .proxyUserId(generateId())
        .build());

    proxiesClient.attemptToUpdateProxyRelationship(ProxyRelationship.builder()
        .id(generateId())
        .userId(generateId())
        .proxyUserId(generateId())
        .build())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void canFindProxiesForUser() {
    final var userId = generateId();
    final var differentUserId = generateId();
    final var firstProxyUserId = generateId();
    final var secondProxyUserId = generateId();

    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(userId)
        .proxyUserId(firstProxyUserId)
        .build());

    proxiesClient.createProxyRelationship(
        ProxyRelationship.builder()
          .userId(userId)
          .proxyUserId(secondProxyUserId)
          .build());

    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(differentUserId)
        .proxyUserId(secondProxyUserId)
        .build());

    final var proxyRelationships = proxiesClient.getProxyRelationships(
      String.format("userId==%s", userId));

    assertThat(proxyRelationships.getTotalRecords(), is(2));

    final var allProxies = proxyRelationships.getProxiesFor()
      .stream()
      .map(ProxyRelationship::getProxyUserId)
      .collect(Collectors.toList());

    assertThat(allProxies, containsInAnyOrder(firstProxyUserId, secondProxyUserId));
  }

  @Test
  void canDeleteRelationship() {
    final var relationship = proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(generateId())
        .proxyUserId(generateId())
        .build());

    proxiesClient.attemptToDeleteProxyRelationship(relationship.getId())
      .statusCode(is(HTTP_NO_CONTENT));

    proxiesClient.attemptToGetProxyRelationship(relationship.getId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  @Test
  void cannotDeleteUnknownRelationship() {
    // Create a relationship to ensure it isn't accidentally deleted
    proxiesClient.createProxyRelationship(
      ProxyRelationship.builder()
        .userId(generateId())
        .proxyUserId(generateId())
        .build());

    proxiesClient.attemptToDeleteProxyRelationship(generateId())
      .statusCode(is(HTTP_NOT_FOUND));
  }

  private static String generateId() {
    return UUID.randomUUID().toString();
  }
}
