package org.folio.service.impl;

import org.folio.service.RecordService;
import org.folio.service.spi.RecordServiceFactory;

import io.vertx.core.Vertx;

public class RecordServiceFactoryImpl implements RecordServiceFactory {

  @Override
  public RecordService create(Vertx vertx) {
    return new RecordServiceImpl(vertx);
  }
}
