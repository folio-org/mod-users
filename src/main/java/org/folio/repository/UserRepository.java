package org.folio.repository;

import static org.folio.rest.persist.PgUtil.postgresClient;

import java.util.Map;

import org.folio.rest.jaxrs.model.User;

import io.vertx.core.Context;

public class UserRepository extends AbstractRepository<User> {
  public static final String USER_TABLE = "users";

  public UserRepository(Context context, Map<String, String> okapiHeaders) {
    super(postgresClient(context, okapiHeaders), USER_TABLE, User.class);
  }
}
