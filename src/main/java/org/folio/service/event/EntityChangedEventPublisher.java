package org.folio.service.event;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.logging.log4j.LogManager.getLogger;
import static org.folio.HttpStatus.HTTP_CREATED;
import static org.folio.HttpStatus.HTTP_NO_CONTENT;
import static org.folio.rest.tools.utils.TenantTool.tenantId;

import java.util.Map;
import java.util.function.Function;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.repository.AbstractRepository;

import io.vertx.core.Future;

public class EntityChangedEventPublisher<K, T> {

  private static final Logger log = getLogger(EntityChangedEventPublisher.class);

  private final Map<String, String> okapiHeaders;
  private final Function<T, K> keyExtractor;
  private final EntityChangedEventFactory<T> eventFactory;
  private final DomainEventPublisher<K, EntityChangedData<T>> eventPublisher;
  private final AbstractRepository<T> repository;

  EntityChangedEventPublisher(Map<String, String> okapiHeaders,
    Function<T, K> keyExtractor, EntityChangedEventFactory<T> eventFactory,
    DomainEventPublisher<K, EntityChangedData<T>> eventPublisher,
    AbstractRepository<T> repository) {

    this.okapiHeaders = okapiHeaders;
    this.keyExtractor = keyExtractor;
    this.eventFactory = eventFactory;
    this.eventPublisher = eventPublisher;
    this.repository = repository;
  }

  @SuppressWarnings("unchecked")
  public Function<Response, Future<Response>> publishCreated() {
    return response -> {
      if (!isCreateSuccessResponse(response)) {
        log.warn("publishCreated:: record create failed, skipping event publishing");

        return succeededFuture(response);
      }

      T entity = (T) response.getEntity();
      return publishCreated(keyExtractor.apply(entity), entity)
        .map(response);
    };
  }

  public Function<Response, Future<Response>> publishUpdated(T oldEntity) {
    return response -> {
      if (!isUpdateSuccessResponse(response)) {
        log.warn("publishUpdated:: record update failed, skipping event publishing");
        return succeededFuture(response);
      }

      K key = keyExtractor.apply(oldEntity);
      return repository.getById(key.toString())
          .compose(newEntity -> publishUpdated(key, oldEntity, newEntity))
          .map(response);
    };
  }

  public Function<Response, Future<Response>> publishRemoved(T oldEntity) {
    return response -> {
      if (!isDeleteSuccessResponse(response)) {
        log.warn("publishRemoved:: record removal failed, skipping event publishing");
        return succeededFuture(response);
      }

      return publishRemoved(keyExtractor.apply(oldEntity), oldEntity)
          .map(response);
    };
  }

  public Future<Void> publishCreated(K key, T newEntity) {
    return eventPublisher.publish(key, eventFactory.created(newEntity, tenantId(okapiHeaders)), okapiHeaders);
  }

  public Future<Void> publishUpdated(K key, T oldEntity, T newEntity) {
    return eventPublisher.publish(key, eventFactory.updated(oldEntity, newEntity, tenantId(okapiHeaders)), okapiHeaders);
  }

  public Future<Void> publishRemoved(K key, T oldEntity) {
    return eventPublisher.publish(key, eventFactory.deleted(oldEntity, tenantId(okapiHeaders)), okapiHeaders);
  }

  public boolean isCreateSuccessResponse(Response response) {
    return responseHasStatus(response, HTTP_CREATED);
  }

  private boolean isUpdateSuccessResponse(Response response) {
    return responseHasStatus(response, HTTP_NO_CONTENT);
  }

  public boolean isDeleteSuccessResponse(Response response) {
    return responseHasStatus(response, HTTP_NO_CONTENT);
  }

  private boolean responseHasStatus(Response response, HttpStatus expectedStatus) {
    return response != null && response.getStatus() == expectedStatus.toInt();
  }
}
