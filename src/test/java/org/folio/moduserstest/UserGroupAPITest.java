package org.folio.moduserstest;

import java.util.Collections;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

import org.folio.postgres.testing.PostgresTesterContainer;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.folio.rest.impl.UserGroupAPI;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.VertxUtils;

@RunWith(VertxUnitRunner.class)
public class UserGroupAPITest {
  private static Vertx vertx = VertxUtils.getVertxWithExceptionHandler();
  private static Context vertxContext = vertx.getOrCreateContext();

  public class MyUserGroupAPI extends UserGroupAPI {
    @Override
    protected boolean isDuplicate(String errorMessage) {
      throw new RuntimeException("testing exception handling");
    }
  }

  @BeforeClass
  public static void setup() {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());
  }

  @Test
  public void postException(TestContext context) {
    new MyUserGroupAPI().postGroups("en", new Usergroup(), Collections.emptyMap(), context.asyncAssertSuccess(result -> {
      context.assertTrue(result.getEntity().toString().toLowerCase().contains("internal server error"));
    }), vertxContext);
  }
}
