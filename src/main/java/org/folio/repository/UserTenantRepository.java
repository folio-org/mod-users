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
import java.util.Objects;
import java.util.Optional;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

public class UserTenantRepository {

  public static final String USER_ID_FIELD = "user_id";
  public static final String USERNAME_FIELD = "username";
  public static final String TENANT_ID_FIELD = "tenant_id";
  public static final String CENTRAL_TENANT_ID = "central_tenant_id";
  public static final String BARCODE = "barcode";
  public static final String EXTERNAL_SYSTEM_ID = "external_system_id";
  public static final String CONSORTIUM_ID = "consortium_id";
  public static final String LOWERCASE_WRAPPED_USERNAME = String.format("LOWER(%s)", USERNAME_FIELD);
  public static final String PHONE_NUMBER = "phone_number";
  public static final String MOBILE_PHONE_NUMBER = "mobile_phone_number";
  public static final String EMAIL = "email";
  public static final String UNIQUE_CONSTRAINT_VIOLATION_CODE = "23505";

  private static final String USER_TENANT_TABLE_NAME = "user_tenant";
  private static final String ID_FIELD = "id";
  private static final String TOTAL_COUNT_FIELD = "total_count";
  private static final String INSERT_SQL = "INSERT INTO %s.%s (id, user_id, username, tenant_id, creation_date, central_tenant_id, email, mobile_phone_number, phone_number, barcode, external_system_id, consortium_id)" +
    " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)";
  private static final String DELETE_SQL = "DELETE FROM %s.%s WHERE user_id = $1 AND tenant_id = $2";
  private static final String UPDATE_SQL = "UPDATE %s.%s SET username = $1, email = $2, mobile_phone_number = $3, phone_number = $4, barcode = $5, external_system_id = $6 WHERE user_id = $7";
  private static final String SELECT_USER_TENANTS = "WITH cte AS (SELECT count(*) AS total_count FROM %1$s.%2$s %3$s) " +
    "SELECT j.*, cte.* FROM %1$s.%2$s j LEFT JOIN cte ON true " +
    "%3$s LIMIT %4$d OFFSET %5$d";
  private static final String IS_USERNAME_ALREADY_EXISTS = "SELECT EXISTS(SELECT 1 FROM %s.%s WHERE username = $1)";

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

  public Future<Boolean> isUsernameAlreadyExists(Conn conn, String username, String tenantId) {
    String query = String.format(IS_USERNAME_ALREADY_EXISTS, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME);
    Tuple queryParams = Tuple.of(username);
    return conn.execute(query, queryParams).map(resultSet -> resultSet.iterator().next().getBoolean(0));
  }

  public Future<Boolean> saveUserTenant(Conn conn, UserTenant userTenant, String tenantId) {
    String query = String.format(INSERT_SQL, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME);
    Tuple queryParams = Tuple.of(userTenant.getId(), userTenant.getUserId(), userTenant.getUsername(),
      userTenant.getTenantId(), OffsetDateTime.now(Clock.systemUTC()), userTenant.getCentralTenantId(),
      userTenant.getEmail(), userTenant.getMobilePhoneNumber(), userTenant.getPhoneNumber(), userTenant.getBarcode(),
      userTenant.getExternalSystemId(), userTenant.getConsortiumId());
    return conn.execute(query, queryParams)
      .map(resultSet -> resultSet.size() == 1)
      .recover(throwable -> handleFailures(throwable, userTenant.getId()));
  }

  public Future<Boolean> updateUserTenant(Conn conn, UserTenant userTenant, String tenantId) {
    String query = String.format(UPDATE_SQL, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME);
    Tuple queryParams = Tuple.of(userTenant.getUsername(), userTenant.getEmail(), userTenant.getMobilePhoneNumber(),
      userTenant.getPhoneNumber(), userTenant.getBarcode(), userTenant.getExternalSystemId(), userTenant.getUserId());
    return conn.execute(query, queryParams)
      .map(resultSet -> resultSet.size() == 1)
      .recover(throwable -> handleFailures(throwable, userTenant.getId()));
  }

  public Future<Boolean> deleteUserTenant(Conn conn, UserTenant userTenant, String tenantId) {
    String query = String.format(DELETE_SQL, convertToPsqlStandard(tenantId), USER_TENANT_TABLE_NAME);
    Tuple queryParams = Tuple.of(userTenant.getUserId(), userTenant.getTenantId());
    return conn.execute(query, queryParams)
      .map(resultSet -> resultSet.size() == 1)
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
    return (throwable instanceof PgException pgException && pgException.getCode().equals(UNIQUE_CONSTRAINT_VIOLATION_CODE)) ?
      Future.failedFuture(new DuplicateEventException(String.format("Event with Id=%s is already processed.", id))) :
      Future.failedFuture(throwable);
  }

  private UserTenant mapRowToUserTenant(Row row) {
    return new UserTenant()
      .withId(row.getValue(ID_FIELD).toString())
      .withUserId(row.getValue(USER_ID_FIELD).toString())
      .withUsername(row.getString(USERNAME_FIELD))
      .withTenantId(row.getString(TENANT_ID_FIELD))
      .withEmail(row.getString(EMAIL))
      .withPhoneNumber(row.getString(PHONE_NUMBER))
      .withMobilePhoneNumber(row.getString(MOBILE_PHONE_NUMBER))
      .withCentralTenantId(row.getString(CENTRAL_TENANT_ID))
      .withBarcode(row.getString(BARCODE))
      .withExternalSystemId(row.getString(EXTERNAL_SYSTEM_ID))
      .withConsortiumId(Objects.nonNull(row.getUUID(CONSORTIUM_ID)) ? row.getValue(CONSORTIUM_ID).toString() : null);
  }
}
