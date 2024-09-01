package org.folio.service.event;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class DomainEvent<T> {

  private final UUID id;
  private final DomainEventType type;
  private final String tenant;
  private final long timestamp;
  private final T data;

  public DomainEvent(UUID id, DomainEventType type, String tenant, long timestamp, T data) {
    this.id = id;
    this.type = type;
    this.tenant = tenant;
    this.timestamp = timestamp;
    this.data = data;
  }

  public UUID getId() {
    return this.id;
  }

  public DomainEventType getType() {
    return this.type;
  }

  public String getTenant() {
    return this.tenant;
  }

  public long getTimestamp() {
    return this.timestamp;
  }

  public T getData() {
    return this.data;
  }

  public static <T> Builder<T> builder() {
    return new Builder<>();
  }

  public static class Builder<T> {
    private UUID id;
    private DomainEventType type;
    private String tenant;
    private long timestamp;
    private T data;

    public Builder<T> id(UUID id) {
      this.id = id;
      return this;
    }

    public Builder<T> type(DomainEventType type) {
      this.type = type;
      return this;
    }

    public Builder<T> tenant(String tenant) {
      this.tenant = tenant;
      return this;
    }

    public Builder<T> timestamp(long timestamp) {
      this.timestamp = timestamp;
      return this;
    }

    public Builder<T> data(T data) {
      this.data = data;
      return this;
    }

    public DomainEvent<T> build() {
      return new DomainEvent<>(id, type, tenant, timestamp, data);
    }
  }
}
