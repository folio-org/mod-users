package org.folio.repository;

import static io.vertx.core.Future.succeededFuture;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import org.jspecify.annotations.Nullable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;

import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.persist.interfaces.Results;
import org.folio.support.tags.UnitTest;

@UnitTest
@ExtendWith({ VertxExtension.class })
class AbstractRepositoryTest {

  private static final String TEST_TABLE = "test_table";

  private TestRepository<TestEntity> testRepository;
  private PostgresClient postgresClient;

  @BeforeEach
  void setUp() {
    postgresClient = Mockito.mock(PostgresClient.class);
    testRepository = new TestRepository<>(postgresClient, TEST_TABLE, TestEntity.class);
  }

  @AfterEach
  void tearDown() {
    Mockito.reset(postgresClient);
  }

  @Test
  void save_positive(VertxTestContext testContext) {
    var id = randomId();
    var entity = new TestEntity(id);
    when(postgresClient.save(TEST_TABLE, id, entity)).thenReturn(succeededFuture(id));

    testRepository.save(id, entity).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(id));
      testContext.completeNow();
    }));
  }

  @Test
  void getById_positive(VertxTestContext testContext) {
    var id = randomId();
    var entity = new TestEntity(id);
    when(postgresClient.getById(TEST_TABLE, id, TestEntity.class)).thenReturn(succeededFuture(entity));

    testRepository.getById(id).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(entity));
      testContext.completeNow();
    }));
  }

  @Test
  void get_positive_withCriterion(VertxTestContext testContext) {
    var criterion = new Criterion();
    var entity1 = new TestEntity(randomId());
    var entity2 = new TestEntity(randomId());
    var results = new Results<TestEntity>();
    results.setResults(List.of(entity1, entity2));

    when(postgresClient.get(TEST_TABLE, TestEntity.class, criterion, false))
      .thenReturn(succeededFuture(results));

    testRepository.get(criterion).onComplete(testContext.succeeding(result -> {
      assertThat(result, hasSize(2));
      assertThat(result, containsInAnyOrder(entity1, entity2));
      testContext.completeNow();
    }));
  }

  @Test
  void get_positive_connectionAndCriterion(VertxTestContext testContext) {
    var criterion = new Criterion();
    var id1 = randomId();
    var entity1 = new TestEntity(id1);
    var sqlConnection = Mockito.mock(SQLConnection.class);
    var connection = succeededFuture(sqlConnection);

    var results = new Results<>();
    results.setResults(List.of(entity1));

    doAnswer(invocation -> testHandler(invocation, results, 6))
      .when(postgresClient)
      .get(any(), eq(TEST_TABLE), eq(TestEntity.class), eq(criterion), anyBoolean(), anyBoolean(), any());

    testRepository.get(connection, criterion).onComplete(testContext.succeeding(result -> {
      assertThat(result, hasSize(1));
      assertThat(result, contains(entity1));
      testContext.completeNow();
    }));
  }

  @Test
  void getById_positive_withCollection(VertxTestContext testContext) {
    var id1 = randomId();
    var id2 = randomId();
    var entity1 = new TestEntity(id1);
    var entity2 = new TestEntity(id2);

    var resultMap = Map.of(id1, entity1, id2, entity2);
    var expectedQueryIds = new JsonArray(List.of(id1, id2));

    doAnswer(invocation -> testHandler(invocation, resultMap, 3))
      .when(postgresClient)
      .getById(eq(TEST_TABLE), eq(expectedQueryIds), eq(TestEntity.class), any());

    testRepository.getById(List.of(id1, id2)).onComplete(testContext.succeeding(result -> {
      assertThat(result, notNullValue());
      assertThat(result.get(id1), is(entity1));
      assertThat(result.get(id2), is(entity2));
      testContext.completeNow();
    }));
  }

  @Test
  void getById_positive_withCollectionAndMapper(VertxTestContext testContext) {
    var id1 = randomId();
    var id2 = randomId();
    var entity1 = new TestEntity(id1);
    var entity2 = new TestEntity(id2);

    var resultMap = Map.of(id1, entity1, id2, entity2);

    var expectedQueryIds = new JsonArray(List.of(id1, id2));
    doAnswer(invocation -> testHandler(invocation, resultMap, 3))
      .when(postgresClient)
      .getById(eq(TEST_TABLE), eq(expectedQueryIds), eq(TestEntity.class), any());

    var records = List.of(entity1, entity2);
    testRepository.getById(records, TestEntity::id).onComplete(testContext.succeeding(result -> {
      assertThat(result, notNullValue());
      assertThat(result.get(id1), is(entity1));
      assertThat(result.get(id2), is(entity2));
      testContext.completeNow();
    }));
  }

  @Test
  void update_positive_withAsyncConnection_(VertxTestContext testContext) {
    var id = randomId();
    var entity = new TestEntity(id);
    var rowSet = Mockito.<RowSet<Row>>mock();
    var sqlConnection = Mockito.mock(SQLConnection.class);
    var connection = succeededFuture(sqlConnection);

    doAnswer(invocation -> testHandler(invocation, rowSet, 6))
      .when(postgresClient)
      .update(any(), eq(TEST_TABLE), eq(entity), eq("jsonb"), anyString(), anyBoolean(), any());

    testRepository.update(connection, id, entity).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  @Test
  void update_positive_withSQLConnection(VertxTestContext testContext) {
    var id = randomId();
    var sqlConnection = Mockito.mock(SQLConnection.class);
    var entity = new TestEntity(id);
    var rowSet = Mockito.<RowSet<Row>>mock();

    doAnswer(invocation -> testHandler(invocation, rowSet, 6))
      .when(postgresClient)
      .update(any(), eq(TEST_TABLE), eq(entity), eq("jsonb"), anyString(), anyBoolean(), any());

    testRepository.update(sqlConnection, id, entity).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  @Test
  void update_positive_withId(VertxTestContext testContext) {
    var id = randomId();
    var entity = new TestEntity(id);
    var rowSet = Mockito.<RowSet<Row>>mock();

    when(postgresClient.update(TEST_TABLE, entity, id)).thenReturn(succeededFuture(rowSet));

    testRepository.update(id, entity).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  @Test
  void update_positive_batchRequest(VertxTestContext testContext) {
    var entity1 = new TestEntity(randomId());
    var entity2 = new TestEntity(randomId());
    var records = List.of(entity1, entity2);
    var rowSet = Mockito.<RowSet<Row>>mock();

    when(postgresClient.upsertBatch(TEST_TABLE, records)).thenReturn(succeededFuture(rowSet));

    testRepository.update(records).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  @Test
  void upsert_positive(VertxTestContext testContext) {
    var id = randomId();
    var entity = new TestEntity(id);

    when(postgresClient.upsert(TEST_TABLE, id, entity)).thenReturn(succeededFuture(id));

    testRepository.upsert(id, entity).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(id));
      testContext.completeNow();
    }));
  }

  @Test
  void deleteAll_positive(VertxTestContext testContext) {
    var rowSet = Mockito.<RowSet<Row>>mock();
    when(postgresClient.delete(eq(TEST_TABLE), any(Criterion.class)))
      .thenReturn(succeededFuture(rowSet));

    testRepository.deleteAll().onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  @Test
  void deleteById_positive(VertxTestContext testContext) {
    var id = randomId();
    var rowSet = Mockito.<RowSet<Row>>mock();

    when(postgresClient.delete(TEST_TABLE, id)).thenReturn(succeededFuture(rowSet));

    testRepository.deleteById(id).onComplete(testContext.succeeding(result -> {
      assertThat(result, is(rowSet));
      testContext.completeNow();
    }));
  }

  private static String randomId() {
    return UUID.randomUUID().toString();
  }

  private static <T> @Nullable Object testHandler(InvocationOnMock invocation, T resultMap, int argPos) {
    var handler = invocation.<Handler<AsyncResult<T>>>getArgument(argPos);
    handler.handle(succeededFuture(resultMap));
    return null;
  }

  private record TestEntity(String id) {}

  private static class TestRepository<T> extends AbstractRepository<T> {

    protected TestRepository(PostgresClient postgresClient, String tableName, Class<T> recordType) {
      super(postgresClient, tableName, recordType);
    }
  }
}
