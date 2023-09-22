package org.folio.moduserstest;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.BeforeAll;

public class AbstractRestTestNoData extends AbstractRestTest {

  @BeforeAll
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    AbstractRestTest.beforeAll(vertx, context, false, KAFKA_CONTAINER_PORTS);
  }
}
