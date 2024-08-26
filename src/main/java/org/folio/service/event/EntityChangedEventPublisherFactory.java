package org.folio.service.event;

import static org.folio.rest.tools.utils.TenantTool.tenantId;
import static org.folio.support.kafka.topic.UsersKafkaTopic.USER_GROUP;
import static org.folio.support.kafka.topic.UsersKafkaTopic.USER_UPDATED;

import java.util.Map;

import org.folio.repository.UserGroupRepository;
import org.folio.repository.UserRepository;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.Usergroup;

import io.vertx.core.Context;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class EntityChangedEventPublisherFactory {

  private EntityChangedEventPublisherFactory() {
  }

  public static EntityChangedEventPublisher<String, Usergroup> userGroupEventPublisher(
    Context vertxContext, Map<String, String> okapiHeaders) {

    return new EntityChangedEventPublisher<>(okapiHeaders, Usergroup::getId,
      new EntityChangedEventFactory<>(), new DomainEventPublisher<>(vertxContext,
      USER_GROUP.fullTopicName(tenantId(okapiHeaders)), FailureHandler.noOperation()),
      new UserGroupRepository(vertxContext, okapiHeaders));
  }

  public static EntityChangedEventPublisher<String, User> userEventPublisher(
    Context vertxContext, Map<String, String> okapiHeaders) {

    return new EntityChangedEventPublisher<>(okapiHeaders, User::getId,
      new EntityChangedEventFactory<>(), new DomainEventPublisher<>(vertxContext,
      USER_UPDATED.fullTopicName(tenantId(okapiHeaders)), FailureHandler.noOperation()),
      new UserRepository(vertxContext, okapiHeaders));
  }
}
