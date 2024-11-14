package org.folio.repository;

import static org.folio.rest.persist.PgUtil.postgresClient;

import java.util.Map;

import org.folio.rest.jaxrs.model.Usergroup;

import io.vertx.core.Context;

public class UserGroupRepository extends AbstractRepository<Usergroup> {
  public static final String GROUP_TABLE = "groups";

  public UserGroupRepository(Context context, Map<String, String> okapiHeaders) {
    super(postgresClient(context, okapiHeaders), GROUP_TABLE, Usergroup.class);
  }
}
