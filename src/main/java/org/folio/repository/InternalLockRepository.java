package org.folio.repository;

import io.vertx.core.Future;
import io.vertx.sqlclient.SqlResult;
import io.vertx.sqlclient.Tuple;
import org.folio.rest.persist.Conn;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

public class InternalLockRepository {

  private static final String TABLE_NAME = "internal_lock";
  private static final String SELECT_WITH_LOCKING = "SELECT * FROM %s.%s WHERE lock_name = $1 FOR UPDATE";

  /**
   * Performs SELECT FOR UPDATE statement in order to implement locking.
   * Lock released after transaction's commit.
   *
   * @param conn connection with active transaction
   * @param lockName the lock name
   * @param tenantId the tenant id
   * @return future with 1 row if lock was acquired
   */
  public Future<Integer> selectWithLocking(Conn conn, String lockName, String tenantId) {
    String query = String.format(SELECT_WITH_LOCKING, convertToPsqlStandard(tenantId), TABLE_NAME);
    Tuple params = Tuple.of(lockName);
    return conn.execute(query, params).map(SqlResult::rowCount);
  }
}
