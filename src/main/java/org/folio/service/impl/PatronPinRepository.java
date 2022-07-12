package org.folio.service.impl;

import static org.folio.rest.impl.PatronPinAPI.TABLE_NAME_PATRON_PIN;

import org.folio.rest.jaxrs.model.Patronpin;
import org.folio.rest.persist.PostgresClient;

import io.vertx.core.Future;

public class PatronPinRepository {
  private final PostgresClient postgresClient;

  public PatronPinRepository(PostgresClient postgresClient) {
    this.postgresClient = postgresClient;
  }

  public Future<String> savePin(Patronpin patronPin) {
    return postgresClient.save(TABLE_NAME_PATRON_PIN, patronPin.getId(),
      patronPin, false, true);
  }
}
