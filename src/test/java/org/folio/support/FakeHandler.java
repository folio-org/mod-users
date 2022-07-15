package org.folio.support;

import io.vertx.core.Handler;

public class FakeHandler<T> implements Handler<T> {
  public T handledObject;

  @Override
  public void handle(T o) {
    handledObject = o;
  }
}
