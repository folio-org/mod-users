package org.folio.service.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import io.vertx.sqlclient.RowSet;
import lombok.SneakyThrows;
import org.folio.exceptions.UsersSettingsException;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.Setting;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.interfaces.Results;
import org.folio.service.UsersSettingsService;
import org.folio.support.tags.UnitTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@UnitTest
@ExtendWith({VertxExtension.class, MockitoExtension.class})
class UsersSettingsServiceTest {

  @Mock
  private Conn conn;

  private UsersSettingsService service;

  @BeforeEach
  void setUp() {
    service = new UsersSettingsService();
  }

  @SneakyThrows
  @Test
  void testUpdateSetting_Success(VertxTestContext testContext) {
    var id = UUID.randomUUID().toString();
    var oldSetting = createSetting(id, "old-value");
    var newSetting = createSetting(id, "new-value");

    when(conn.getById(anyString(), eq(id), eq(Setting.class)))
      .thenReturn(Future.succeededFuture(oldSetting));
    when(conn.update(anyString(), any(Setting.class), eq(id)))
      .thenReturn(Future.succeededFuture(mock(RowSet.class)));

    service.updateSetting(conn, id, newSetting)
      .onComplete(testContext.succeeding(response -> testContext.verify(() -> {
        assertEquals(Response.Status.NO_CONTENT.getStatusCode(), response.getStatus());
        verify(conn).getById(anyString(), eq(id), eq(Setting.class));
        verify(conn).update(anyString(), any(Setting.class), eq(id));
        testContext.completeNow();
      })));
  }

  @Test
  void testUpdateSetting_NotFound(VertxTestContext testContext) {
    var id = UUID.randomUUID().toString();
    var newSetting = createSetting(id, "new-value");

    when(conn.getById(anyString(), eq(id), eq(Setting.class)))
      .thenReturn(Future.succeededFuture(null));

    service.updateSetting(conn, id, newSetting)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        assertInstanceOf(UsersSettingsException.class, throwable);
        assertTrue(throwable.getMessage().contains(id));
        testContext.completeNow();
      })));
  }

  @Test
  void testUpdateSetting_IdMismatch(VertxTestContext testContext) {
    var id = UUID.randomUUID().toString();
    var differentId = UUID.randomUUID().toString();
    var oldSetting = createSetting(id, "old-value");
    var newSetting = createSetting(differentId, "new-value");

    when(conn.getById(anyString(), eq(id), eq(Setting.class)))
      .thenReturn(Future.succeededFuture(oldSetting));

    service.updateSetting(conn, id, newSetting)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        assertInstanceOf(UsersSettingsException.class, throwable);
        assertTrue(throwable.getMessage().contains("not equal to id"));
        testContext.completeNow();
      })));
  }

  @Test
  void testUpdateSetting_ProfilePicture_InvalidEncryptionKey(VertxTestContext testContext) {
    var id = UUID.randomUUID().toString();
    var oldConfig = new Config().withEncryptionKey("old-key");
    var newConfig = new Config().withEncryptionKey(null);

    var oldSetting = createProfilePictureSetting(id, oldConfig);
    var newSetting = createProfilePictureSetting(id, newConfig);

    when(conn.getById(anyString(), eq(id), eq(Setting.class)))
      .thenReturn(Future.succeededFuture(oldSetting));


    service.updateSetting(conn, id, newSetting)
      .onComplete(testContext.succeeding(response -> testContext.verify(() -> {
        assertEquals(Response.Status.BAD_REQUEST.getStatusCode(), response.getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testUpdateSetting_ProfilePicture_ExceedsMaxFileSize(VertxTestContext testContext) {
    var id = UUID.randomUUID().toString();
    var oldConfig = new Config().withEncryptionKey("key").withMaxFileSize(5.0);
    var newConfig = new Config().withEncryptionKey("key").withMaxFileSize(15.0);

    var oldSetting = createProfilePictureSetting(id, oldConfig);
    var newSetting = createProfilePictureSetting(id, newConfig);

    when(conn.getById(anyString(), eq(id), eq(Setting.class)))
      .thenReturn(Future.succeededFuture(oldSetting));

    service.updateSetting(conn, id, newSetting)
      .onComplete(testContext.succeeding(response -> testContext.verify(() -> {
        assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testGetProfilePictureConfigSetting_Success(VertxTestContext testContext) {
    var config = new Config().withMaxFileSize(5.0).withEncryptionKey("test-key");
    var setting = createProfilePictureSetting(UUID.randomUUID().toString(), config);
    var results = new Results<Setting>();
    results.setResults(List.of(setting));

    when(conn.get(anyString(), eq(Setting.class), any(Criterion.class)))
      .thenReturn(Future.succeededFuture(results));

    service.getProfilePictureConfigSetting(conn)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertNotNull(result);
        assertEquals(5, result.getMaxFileSize());
        assertEquals("test-key", result.getEncryptionKey());
        testContext.completeNow();
      })));
  }

  @Test
  void testGetProfilePictureConfigSetting_NotFound(VertxTestContext testContext) {
    var results = new Results<Setting>();
    results.setResults(List.of());

    when(conn.get(anyString(), eq(Setting.class), any(Criterion.class)))
      .thenReturn(Future.succeededFuture(results));

    service.getProfilePictureConfigSetting(conn)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        assertInstanceOf(UsersSettingsException.class, throwable);
        testContext.completeNow();
      })));
  }

  @Test
  void testCreateProfilePictureConfigSetting_Success(VertxTestContext testContext) {
    var config = new Config()
      .withMaxFileSize(5.0)
      .withEncryptionKey("test-key")
      .withConfigName("should-be-removed");

    when(conn.save(anyString(), anyString(), any(Setting.class)))
      .thenReturn(Future.succeededFuture("new-id"));

    var savedSetting = createProfilePictureSetting("new-id", config);
    var results = new Results<Setting>();
    results.setResults(List.of(savedSetting));

    when(conn.get(anyString(), eq(Setting.class), any(Criterion.class)))
      .thenReturn(Future.succeededFuture(results));

    service.createProfilePictureConfigSetting(conn, config)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertNotNull(result);
        assertEquals(5, result.getMaxFileSize());
        assertEquals("test-key", result.getEncryptionKey());
        verify(conn).save(anyString(), anyString(), any(Setting.class));
        testContext.completeNow();
      })));
  }

  // Helper methods using Java 21 features
  private Setting createSetting(String id, String key) {
    return new Setting()
      .withId(id)
      .withKey(key)
      .withScope(Setting.Scope.MOD_USERS)
      .withValue(Map.of("data", "test"));
  }

  private Setting createProfilePictureSetting(String id, Config config) {
    var configMap = JsonObject.mapFrom(config).getMap();
    configMap.remove("id");
    configMap.remove("configName");
    configMap.remove("metadata");

    return new Setting()
      .withId(id)
      .withKey("PROFILE_PICTURE_CONFIG")
      .withScope(Setting.Scope.MOD_USERS)
      .withValue(configMap);
  }
}

