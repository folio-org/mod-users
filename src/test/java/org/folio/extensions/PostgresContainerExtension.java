package org.folio.extensions;

import static org.folio.postgres.testing.PostgresTesterContainer.DEFAULT_IMAGE_NAME;

import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.testcontainers.containers.PostgreSQLContainer;

import org.folio.rest.tools.utils.Envs;

public class PostgresContainerExtension implements BeforeAllCallback, AfterAllCallback {

  public static final String DB_USERNAME = "folio";
  public static final String DB_PASSWORD = "folio-test-password";
  public static final String DB_DATABASE = "mod-users-it";

  @SuppressWarnings("resource")
  private static final PostgreSQLContainer<?> CONTAINER =
    new PostgreSQLContainer<>(DEFAULT_IMAGE_NAME)
      .withDatabaseName(DB_DATABASE)
      .withEnv("PG_USER", DB_USERNAME)
      .withUsername(DB_USERNAME)
      .withPassword(DB_PASSWORD);

  @Override
  public void beforeAll(ExtensionContext context) throws Exception {
    if (!CONTAINER.isRunning()) {
      CONTAINER.start();
    }

    var host = CONTAINER.getHost();
    var port = CONTAINER.getMappedPort(5432);
    Envs.setEnv(host, port, DB_USERNAME, DB_PASSWORD, DB_DATABASE);
  }

  @Override
  public void afterAll(ExtensionContext context) {
    /* there is no need to stop the container,
       it will be stopped automatically by Testcontainers
    */
  }
}
