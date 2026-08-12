package org.folio.repository;

import static org.folio.rest.persist.PgUtil.postgresClient;

import java.util.Map;

import org.folio.rest.jaxrs.model.Setting;

import io.vertx.core.Context;

public class SettingsRepository extends AbstractRepository<Setting> {
  public static final String SETTINGS_TABLE = "settings";

  public SettingsRepository(Context context, Map<String, String> okapiHeaders) {
    super(postgresClient(context, okapiHeaders), SETTINGS_TABLE, Setting.class);
  }
}
