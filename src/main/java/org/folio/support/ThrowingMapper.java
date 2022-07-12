package org.folio.support;

@FunctionalInterface
public interface ThrowingMapper<S, T, E extends Exception> {
  T map(S input) throws E;
}
