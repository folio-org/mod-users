package org.folio.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;

import java.util.List;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.vertx.core.json.JsonObject;
import lombok.SneakyThrows;

class JsonUserDeserializationTests {
  @Test
  @SneakyThrows
  void canDeserializeJson() {
    final var json = new JsonObject()
      .put("username", "foo")
      .put("personal", new JsonObject().put("addresses", List.of(new JsonObject().put("addressTypeId", "12345"))))
      .encodePrettily();

    final var user = new ObjectMapper().readValue(json, User.class);

    assertThat(user.getUsername(), is("foo"));
    assertThat(user.getPersonal().getAddresses(), hasSize(1));
    assertThat(user.getPersonal().getAddresses().get(0).getAddressTypeId(), is("12345"));
  }
}
