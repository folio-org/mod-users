package org.folio.rest.impl;

import java.util.Collections;

import org.folio.rest.jaxrs.model.User;
import org.folio.rest.tools.utils.VertxUtils;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class UsersAPITest {

  private Vertx vertx = VertxUtils.getVertxWithExceptionHandler();
  private Context context = vertx.getOrCreateContext();

  @Test
  public void testSaveUserWithEmptyUsername(TestContext testContext) {
    String expectedErrorMsg = "The username must not be blank";
    int expectedErrorStatus = 400;

    new UsersAPI().postUsers("en", new User(), Collections.emptyMap(),
      testContext.asyncAssertSuccess(result -> {
        String message = result.getEntity().toString();
        testContext.assertEquals(expectedErrorMsg, message);
        testContext.assertEquals(expectedErrorStatus, result.getStatus());
      }), context);
  }
}
