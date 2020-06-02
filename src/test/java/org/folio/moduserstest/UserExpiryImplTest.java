package org.folio.moduserstest;

import org.folio.rest.impl.UserExpiryImpl;
import org.folio.rest.tools.utils.VertxUtils;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)

public class UserExpiryImplTest {

  private static Vertx vertx = VertxUtils.getVertxWithExceptionHandler();
  private static Context vertxContext = vertx.getOrCreateContext();
 

  @Test
  public void postException() {
    new UserExpiryImpl().run(vertx, vertxContext);
  }

}