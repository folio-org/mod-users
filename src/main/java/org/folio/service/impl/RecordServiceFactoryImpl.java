package org.folio.service.impl;

import io.vertx.core.Vertx;

import org.folio.service.RecordService;
import org.folio.service.spi.RecordServiceFactory;

public class RecordServiceFactoryImpl implements RecordServiceFactory {

  @Override
  public RecordService create(Vertx vertx) {
    return new RecordServiceImpl(vertx);
  }

}
