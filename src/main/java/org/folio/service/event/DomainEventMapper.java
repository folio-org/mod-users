package org.folio.service.event;

import static java.util.Objects.requireNonNull;

import java.util.UUID;
import java.util.function.Function;

import io.vertx.core.json.JsonObject;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DomainEventMapper {

  public static <T> DomainEvent<T> toDomainEvent(String eventJsonString,
    Function<JsonObject, T> dataMapper) {

    final JsonObject eventJson = new JsonObject(eventJsonString);

    return new DomainEvent<>(
      UUID.fromString(requireNonNull(eventJson.getString("id"))),
      requireNonNull(DomainEventType.valueOf(eventJson.getString("type"))),
      requireNonNull(eventJson.getString("tenant")),
      requireNonNull(eventJson.getLong("timestamp")),
      dataMapper.apply(requireNonNull(eventJson.getJsonObject("data")))
    );
  }

  public static DomainEvent<EntityChangedData<JsonObject>> toEntityChangedEvent(String eventJsonString) {
    return toDomainEvent(eventJsonString, data ->
      new EntityChangedData<>(requireNonNull(data.getJsonObject("old")),
        requireNonNull(data.getJsonObject("new"))));
  }
}
