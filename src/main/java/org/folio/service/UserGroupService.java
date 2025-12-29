package org.folio.service;

import static org.folio.repository.UserGroupRepository.GROUP_TABLE;
import static org.folio.rest.persist.PgUtil.postgresClient;
import static org.folio.rest.utils.ResultHandlerUtils.getAsyncResultHandler;
import static org.folio.service.event.EntityChangedEventPublisherFactory.userGroupEventPublisher;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Usergroup;
import org.folio.rest.jaxrs.model.Usergroups;
import org.folio.rest.jaxrs.resource.Groups;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.service.event.EntityChangedEventPublisher;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;

public class UserGroupService {

  private static final Logger log = LogManager.getLogger(UserGroupService.class);
  private final Context vertxContext;
  private final Map<String, String> okapiHeaders;
  private final PostgresClient postgresClient;
  private final EntityChangedEventPublisher<String, Usergroup> eventPublisher;

  public UserGroupService(Context vertxContext, Map<String, String> okapiHeaders) {
    this.vertxContext = vertxContext;
    this.okapiHeaders = okapiHeaders;

    this.postgresClient = postgresClient(vertxContext, okapiHeaders);
    this.eventPublisher = userGroupEventPublisher(vertxContext, okapiHeaders);
  }

  public Future<Response> findByQuery(String query, int offset, int limit) {
    log.debug("findByQuery:: parameters query: {}, offset: {}, limit: {}", query, offset, limit);

    return PgUtil.get(GROUP_TABLE, Usergroup.class, Usergroups.class, query, offset, limit,
      okapiHeaders, vertxContext, Groups.GetGroupsResponse.class);
  }

  public Future<Response> create(Usergroup userGroup) {
    log.debug("create:: parameters userGroup: {}", () -> userGroup);
    Promise<Response> createResult = Promise.promise();
    PgUtil.post(GROUP_TABLE, userGroup, okapiHeaders, vertxContext,
      Groups.PostGroupsResponse.class, getAsyncResultHandler(createResult));

    return createResult.future()
      .compose(eventPublisher.publishCreated());
  }

  public Future<Response> findById(String groupId) {
    log.debug("findById:: parameters groupId: {}", groupId);

    return PgUtil.getById(GROUP_TABLE, Usergroup.class, groupId, okapiHeaders, vertxContext,
      Groups.GetGroupsByGroupIdResponse.class);
  }

  public Future<Response> updateById(Usergroup userGroup, String groupId) {
    log.debug("updateById:: parameters userGroup: {}, groupId: {}", () -> userGroup, () -> groupId);

    return postgresClient.getById(GROUP_TABLE, groupId, Usergroup.class)
      .compose(oldUserGroup -> PgUtil.put(GROUP_TABLE, userGroup, groupId, okapiHeaders, vertxContext,
          Groups.PutGroupsByGroupIdResponse.class)
        .compose(eventPublisher.publishUpdated(oldUserGroup)));
  }

  public Future<Response> deleteById(String groupId) {
    log.debug("deleteById:: parameters groupId: {}", groupId);

    return postgresClient.getById(GROUP_TABLE, groupId, Usergroup.class)
      .compose(usergroup -> PgUtil.deleteById(GROUP_TABLE, groupId, okapiHeaders, vertxContext,
          Groups.DeleteGroupsByGroupIdResponse.class)
        .compose(eventPublisher.publishRemoved(usergroup)));
  }
}
