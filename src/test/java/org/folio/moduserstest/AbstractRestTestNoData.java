package org.folio.moduserstest;

import static java.util.concurrent.TimeUnit.SECONDS;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import org.junit.jupiter.api.BeforeAll;

@Timeout(value = 20, timeUnit = SECONDS)
public class AbstractRestTestNoData extends AbstractRestTest {

  @BeforeAll
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    AbstractRestTest.beforeAll(vertx, context, false);
  }
}
