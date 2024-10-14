package org.folio.rest.impl;

import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.jaxrs.model.ContactInfo;
import org.folio.rest.jaxrs.model.StagingUser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

@ExtendWith(VertxExtension.class)
@Timeout(value = 5, timeUnit = TimeUnit.SECONDS)
class StagingUsersAPITest {

  @Test
  void getStagingUsersExceptionInCatch(VertxTestContext vtc) {
    new StagingUsersAPI().getStagingUsers(null, null, null, null, 0, 0, null,
      vtc.succeeding(response -> vtc.verify(() -> {
        assertThat(response.getStatus(), is(500));
        vtc.completeNow();
      })), null);
  }

  @Test
  void postStagingUsersWithNullHeader(VertxTestContext vtc) {
    ContactInfo contactInfo = new ContactInfo().withEmail("abc@test.com");
    StagingUser stagingUser = new StagingUser().withContactInfo(contactInfo);

    stagingUser.setContactInfo(contactInfo);
    new StagingUsersAPI().postStagingUsers(stagingUser, null,
      vtc.succeeding(response -> vtc.verify(() -> {
        assertThat(response.getStatus(), is(500));
        vtc.completeNow();
      })), Vertx.vertx().getOrCreateContext());
  }

  @Test
  void postStagingUsers(VertxTestContext vtc) {
    Map<String, String> okapiHeaders = new HashMap<>();
    okapiHeaders.put("X-Okapi-Tenant", "folio_shared");
    ContactInfo contactInfo = new ContactInfo().withEmail("abc@test.com");
    StagingUser stagingUser = new StagingUser().withContactInfo(contactInfo);

    stagingUser.setContactInfo(contactInfo);
    new StagingUsersAPI().postStagingUsers(stagingUser, okapiHeaders,
      vtc.succeeding(response -> vtc.verify(() -> {
        assertThat(response.getStatus(), is(500));
        vtc.completeNow();
      })), Vertx.vertx().getOrCreateContext());
  }
}

