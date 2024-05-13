package org.folio.service.event;

import static org.folio.rest.tools.utils.TenantTool.tenantId;
import static org.folio.support.kafka.topic.UsersKafkaTopic.USER_GROUP;

import java.util.Map;

import org.folio.repository.UserGroupRepository;
import org.folio.rest.jaxrs.model.Usergroup;

import io.vertx.core.Context;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class EntityChangedEventPublisherFactory {

  private static final String NULL_ID = "00000000-0000-0000-0000-000000000000";

  private EntityChangedEventPublisherFactory() {
  }

  public static EntityChangedEventPublisher<String, Usergroup> userGroupEventPublisher(
      Context vertxContext, Map<String, String> okapiHeaders) {

    return new EntityChangedEventPublisher<>(okapiHeaders, Usergroup::getId, NULL_ID,
      new EntityChangedEventFactory<>(), new DomainEventPublisher<>(vertxContext,
      USER_GROUP.fullTopicName(tenantId(okapiHeaders)), FailureHandler.noOperation()),
      new UserGroupRepository(vertxContext, okapiHeaders));
  }
}
