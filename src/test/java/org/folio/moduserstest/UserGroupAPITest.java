package org.folio.moduserstest;

import java.util.Collections;

import org.folio.rest.impl.UserGroupAPI;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.tools.utils.VertxUtils;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class UserGroupAPITest {
  Vertx vertx = VertxUtils.getVertxWithExceptionHandler();
  Context vertxContext = vertx.getOrCreateContext();

  public class MyUserGroupAPI extends UserGroupAPI {
    @Override
    protected boolean isDuplicate(String errorMessage) {
      throw new RuntimeException("testing exception handling");
    }
  }

  @Test
  public void postException(TestContext context) {
    new MyUserGroupAPI().postGroups("en", new Usergroup(), Collections.emptyMap(), context.asyncAssertSuccess(result -> {
      context.assertTrue(result.getEntity().toString().toLowerCase().contains("internal server error"));
    }), vertxContext);
  }

}
