package org.folio.repository;

import io.vertx.core.Future;
import io.vertx.pgclient.PgException;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import org.apache.commons.lang3.StringUtils;
import org.folio.kafka.exception.DuplicateEventException;
import org.folio.rest.jaxrs.model.UserTenant;
import org.folio.rest.jaxrs.model.UserTenantCollection;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criterion;

import java.time.Clock;
import java.time.OffsetDateTime;
import java.util.Optional;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

public class UserTenantRepository {

  public static final String USER_ID_FIELD = "user_id";
  public static final String USERNAME_FIELD = "username";
  public static final String TENANT_ID_FIELD = "tenant_id";
  public static final String UNIQUE_CONSTRAINT_VIOLATION_CODE = "23505";

  private static final String USER_TENANT_TABLE_NAME = "user_tenant";
  private static final String ID_FIELD = "id";
  private static final String TOTAL_COUNT_FIELD = "total_count";
  private static final String INSERT_SQL = "INSERT INTO %s.%s (id, user_id, username, tenant_id, creation_date) VALUES ($1, $2, $3, $4, $5)";
  private static final String SELECT_USER_TENANTS = "WITH cte AS (SELECT count(*) AS total_count FROM %1$s.%2$s %3$s) " +
    "SELECT j.*, cte.* FROM %1$s.%2$s j LEFT JOIN cte ON true " +
    "%3$s LIMIT %4$d OFFSET %5$d";

  public Future<UserTenantCollection> fetchUserTenants(Conn conn, String tenantId, Criterion criterion) {
    int limit = criterion.getLimit().get();
    int offset = criterion.getOffset().get();
    String whereClause = Optional.ofNullable(criterion.getWhere())
      .filter(StringUtils::isNotBlank)
      .map(clause -> "WHERE " + clause)
      .orElse("");
    String query = String.format(SELECT_USER_TENANTS, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME,
      whereClause, limit, offset);
    return conn.execute(query).map(this::mapResultSetToUserTenantCollection);
  }

  public Future<Boolean> saveUserTenant(Conn conn, UserTenant userTenant, String tenantId) {
    String query = String.format(INSERT_SQL, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME);
    Tuple queryParams = Tuple.of(userTenant.getId(), userTenant.getUserId(), userTenant.getUserName(),
      userTenant.getTenantId(), OffsetDateTime.now(Clock.systemUTC()));
    return conn.execute(query, queryParams).map(resultSet -> resultSet.size() == 1)
      .recover(throwable -> handleFailures(throwable, userTenant.getId()));
  }

  private UserTenantCollection mapResultSetToUserTenantCollection(RowSet<Row> resultSet) {
    UserTenantCollection userTenantCollection = new UserTenantCollection().withTotalRecords(0);
    resultSet.iterator().forEachRemaining(row -> {
      userTenantCollection.getUserTenants().add(mapRowToUserTenant(row));
      userTenantCollection.setTotalRecords(row.getInteger(TOTAL_COUNT_FIELD));
    });
    return userTenantCollection;
  }

  private <T> Future<T> handleFailures(Throwable throwable, String id) {
    return (throwable instanceof PgException && ((PgException) throwable).getCode().equals(UNIQUE_CONSTRAINT_VIOLATION_CODE)) ?
      Future.failedFuture(new DuplicateEventException(String.format("Event with Id=%s is already processed.", id))) :
      Future.failedFuture(throwable);
  }

  private UserTenant mapRowToUserTenant(Row row) {
    return new UserTenant()
      .withId(row.getValue(ID_FIELD).toString())
      .withUserId(row.getValue(USER_ID_FIELD).toString())
      .withUserName(row.getString(USERNAME_FIELD))
      .withTenantId(row.getString(TENANT_ID_FIELD));
  }
}
