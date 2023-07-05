package org.folio.utils;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertxconcurrent.Semaphore;
import org.folio.okapi.common.GenericCompositeFuture;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class HelperUtils {

  private static final int SEMAPHORE_MAX_ACTIVE_THREADS = 5;

  public static <I, O> Future<List<O>> executeWithSemaphores(Collection<I> collection,
                                                             FunctionReturningFuture<I, O> f, Context context) {
    if (collection.isEmpty())
      return Future.succeededFuture(List.of());
    return context.<List<Future<O>>>executeBlocking(promise -> {
      Semaphore semaphore = new Semaphore(SEMAPHORE_MAX_ACTIVE_THREADS, Vertx.currentContext().owner());
      List<Future<O>> futures = new ArrayList<>();
      for (I item : collection) {
        semaphore.acquire(() -> {
          Future<O> future = f.apply(item)
            .onComplete(asyncResult -> semaphore.release());
          futures.add(future);
          if (futures.size() == collection.size()) {
            promise.complete(futures);
          }
        });
      }
    }).compose(HelperUtils::collectResultsOnSuccess);
  }

  /**
   * Wait for all requests completion and collect all resulting objects. In case any failed, complete resulting future with the exception
   * @param futures list of futures and each produces resulting object on completion
   * @param <T> resulting objects type
   * @return Future with resulting objects
   */
  public static <T> Future<List<T>> collectResultsOnSuccess(List<Future<T>> futures) {
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(CompositeFuture::list);
  }

  public interface FunctionReturningFuture<I, O> {
    Future<O> apply(I item);
  }
}
