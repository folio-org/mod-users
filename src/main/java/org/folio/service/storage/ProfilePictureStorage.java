package org.folio.service.storage;

import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.utils.OkapiConnectionParams;
import org.folio.s3.client.FolioS3Client;
import org.folio.s3.exception.S3ClientException;
import org.folio.service.UsersSettingsService;

import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;
import static org.folio.support.ProfilePictureHelper.bytesToMegabytes;
import static org.folio.support.ProfilePictureHelper.calculateHmac;
import static org.folio.support.ProfilePictureHelper.decryptAES;
import static org.folio.support.ProfilePictureHelper.encryptAES;
import static org.folio.support.ProfilePictureHelper.verifyHmac;
import static org.folio.support.UsersApiConstants.BLOB;
import static org.folio.support.UsersApiConstants.CHECKSUM;
import static org.folio.support.UsersApiConstants.CONFIG_NAME;
import static org.folio.support.UsersApiConstants.CREATED_DATE;
import static org.folio.support.UsersApiConstants.DELETE_UNUSED_PROFILE_IDS;
import static org.folio.support.UsersApiConstants.ENABLED;
import static org.folio.support.UsersApiConstants.ENABLED_OBJECT_STORAGE;
import static org.folio.support.UsersApiConstants.ENCRYPTION_KEY;
import static org.folio.support.UsersApiConstants.GET_CONFIGURATION_SQL;
import static org.folio.support.UsersApiConstants.GET_PROFILE_PICTURE_SQL;
import static org.folio.support.UsersApiConstants.ID;
import static org.folio.support.UsersApiConstants.JSONB;
import static org.folio.support.UsersApiConstants.MAX_FILE_SIZE;
import static org.folio.support.UsersApiConstants.MAX_IDS_COUNT;
import static org.folio.support.UsersApiConstants.PROFILE_LINK_IDS;
import static org.folio.support.UsersApiConstants.PROFILE_PICTURE_SETTING_KEY;
import static org.folio.support.UsersApiConstants.SAVE_PROFILE_PICTURE_SQL;
import static org.folio.support.UsersApiConstants.SELECT_USERS_PROFILE_LINK_ID;
import static org.folio.support.UsersApiConstants.SIZE_IN_MB;
import static org.folio.support.UsersApiConstants.TABLE_NAME_CONFIG;
import static org.folio.support.UsersApiConstants.TABLE_NAME_PROFILE_PICTURE;
import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;
import static org.folio.support.UsersApiConstants.UPDATE_PROFILE_PICTURE_SQL;

public class ProfilePictureStorage {
  private static final Logger logger = LogManager.getLogger(ProfilePictureStorage.class);

  private final FolioS3ClientFactory folioS3ClientFactory;
  private final UsersSettingsService settingsService;
  private final PostgresClient postgresClient;

  private String path;

  public ProfilePictureStorage(Vertx vertx, String tenantId) {
    folioS3ClientFactory = new FolioS3ClientFactory();
    settingsService = new UsersSettingsService();
    postgresClient = PostgresClient.getInstance(vertx, tenantId);
  }

  public void storeProfilePictureInObjectStorage(byte[] fileBytes, Map<String, String> okapiHeaders, String profileId,
                        Handler<AsyncResult<Response>> asyncResultHandler) {
    final String TENANT_FOLDER = okapiHeaders.get(OkapiConnectionParams.OKAPI_TENANT_HEADER) + "/";
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
      try {
        if (StringUtils.isNotEmpty(profileId)) {
          path = TENANT_FOLDER + profileId;
          client.getSize(path);
          logger.info("storeProfilePictureInObjectStorage:: Updating file {}", profileId);
          client.write(path, new ByteArrayInputStream(fileBytes), fileBytes.length);
          asyncResultHandler.handle(succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond200WithApplicationJson(new ProfilePicture().withId(UUID.fromString(path.substring(path.lastIndexOf("/") + 1))))));
          }
        else if (Objects.isNull(path)) {
          path = TENANT_FOLDER + UUID.randomUUID();
          logger.info("storeProfilePictureInObjectStorage:: Writing file {}", path);
          client.write(path, new ByteArrayInputStream(fileBytes), fileBytes.length);
          asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(UUID.fromString(path.substring(path.lastIndexOf("/") + 1))))));
        }
      } catch (Exception e) {
        if (e.getMessage().startsWith("Error getting size")) {
          logger.error("storeProfilePictureInDbStorage:: Can not update profile picture in object storage with id {}", profileId);
          asyncResultHandler.handle(succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("Profile picture not found")));
        } else {
          logger.error("storeProfilePictureInDbStorage:: Can not store or update profile picture in object storage {}", e.getLocalizedMessage());
          asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(String.format("Error storing file [%s]", e.getLocalizedMessage()))));
        }
      }
  }

  public void removeProfilePictureFromObjectStorage(Map<String, String> okapiHeaders, String profileId,
                                                 Handler<AsyncResult<Response>> asyncResultHandler) {
    final String TENANT_FOLDER = okapiHeaders.get(OkapiConnectionParams.OKAPI_TENANT_HEADER) + "/";
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
    try {
      path = TENANT_FOLDER + profileId;
      logger.info("storeProfilePictureInObjectStorage:: Removing file {}", profileId);
      client.getSize(path);
      client.remove(path);
      asyncResultHandler.handle(succeededFuture(Users.DeleteUsersProfilePictureByProfileIdResponse.respond204()));
    } catch (Exception e) {
      if (e.getMessage().startsWith("Error getting size")) {
        logger.error("storeProfilePictureInDbStorage:: Can not remove profile picture in object storage with id {}", profileId);
        asyncResultHandler.handle(succeededFuture(Users.DeleteUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("Profile picture not found")));
      } else {
        logger.error("storeProfilePictureInDbStorage:: Can not remove profile picture in object storage {}", e.getMessage());
        asyncResultHandler.handle(succeededFuture(Users.DeleteUsersProfilePictureByProfileIdResponse.respond500WithApplicationJson(String.format("Error removing file [%s]", e.getCause().getMessage()))));
      }
    }
  }

  public void getProfilePictureFromObjectStorage(String fileName,
                                                 Handler<AsyncResult<Response>> asyncResultHandler, Map<String, String> okapiHeaders) {
    final String TENANT_FOLDER = okapiHeaders.get(OkapiConnectionParams.OKAPI_TENANT_HEADER) + "/";
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
    try {
      logger.info("getProfilePictureFromObjectStorage:: Getting profile picture from object storage with id {}", fileName);
      path = TENANT_FOLDER + fileName;
      var object = client.getPresignedUrl(path);
      URL url = new URI(object).toURL();
      HttpURLConnection connection = (HttpURLConnection) url.openConnection();
      connection.setRequestMethod("GET");

      try (InputStream inputStream = connection.getInputStream()) {
        byte[] imageData = inputStream.readAllBytes();
        String base64Image = Base64.getEncoder().encodeToString(imageData);
        asyncResultHandler.handle(succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse
          .respond200WithApplicationJson(new ProfilePicture().withId(UUID.fromString(fileName))
            .withProfilePictureBlob(base64Image))));}
      finally {
        connection.disconnect();
      }
    } catch (Exception e){
      logger.error("getProfilePictureFromObjectStorage:: Can not get profile picture from object storage with id {}", fileName);
      asyncResultHandler.handle(succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond404WithTextPlain(String.format("Error getting file [%s]", e.getMessage()))));
    }
  }

  public void storeProfilePictureInDbStorage(byte[] requestBytesArray, Map<String, String> okapiHeaders,
                                             Handler<AsyncResult<Response>> asyncResultHandler, String encryptionKey) {
    try {
      UUID profileId = UUID.randomUUID();
      byte[] encryptedData = encryptAES(requestBytesArray, encryptionKey);
      byte[] hmac = calculateHmac(encryptedData, encryptionKey);
      JsonObject profilePictureDetails = createProfilePictureDetails(bytesToMegabytes(encryptedData.length));
      Tuple params = Tuple.of(profileId, encryptedData, hmac, profilePictureDetails);
      postgresClient.execute(createInsertQuery(okapiHeaders), params)
        .map(rows -> Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(profileId)))
        .map(Response.class::cast)
        .onComplete(reply -> {
          if (reply.cause() != null) {
            logger.error("storeProfilePictureInDbStorage:: Can not store profile picture in DB with id {}", profileId);
            asyncResultHandler.handle(
              succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(reply.cause().getMessage())));
          } else {
            logger.info("storeProfilePictureInDbStorage:: Profile picture stored in DB with id {}", profileId);
            asyncResultHandler.handle(reply);
          }
        });
    } catch (Exception e) {
      logger.error("storeProfilePictureInDbStorage:: Error encrypting profile picture data: {}", e.getMessage());
      asyncResultHandler.handle(
        succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson("Error encrypting profile picture data")));
    }
  }

  private JsonObject createProfilePictureDetails(double size) {
    return new JsonObject()
      .put(SIZE_IN_MB, size)
      .put(CREATED_DATE, java.time.Instant.now());
  }

  public void getProfilePictureFromDbStorage(String profileId, Handler<AsyncResult<Response>> asyncResultHandler,
                                             Map<String, String> okapiHeaders, String encryptionKey) {
    logger.info("getProfilePictureFromDbStorage:: Getting profile picture from db storage with id {}", profileId);
    Tuple params = Tuple.of(profileId);
    postgresClient.execute(createSelectQuery(okapiHeaders, GET_PROFILE_PICTURE_SQL, TABLE_NAME_PROFILE_PICTURE), params)
      .compose(rows -> {
        if (rows.rowCount() != 0) {
          Row row = rows.iterator().next();
          byte[] encryptedData = row.getBuffer(BLOB).getBytes();
          byte[] storedHmac = row.getBuffer(CHECKSUM).getBytes();
          try {
            if (!verifyHmac(encryptedData, storedHmac, encryptionKey)) {
              return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond400WithApplicationJson("Data integrity check failed, POST and GET must use an identical key"));
            }
          } catch (Exception e) {
            return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond400WithApplicationJson("Invalid algorithm OR key"));
          }
          ProfilePicture profilePicture = mapResultSetToProfilePicture(row, encryptionKey);
          return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond200WithApplicationJson(profilePicture));
        } else {
          return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("No profile picture found for id " + profileId));
        }
      })
      .map(Response.class::cast)
      .onComplete(responseAsyncResult -> {
        if (responseAsyncResult.cause() != null) {
          logger.error("getProfilePictureFromDbStorage:: Can not get profile picture from db storage with id {}", profileId);
          asyncResultHandler.handle(
            succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse
              .respond400WithApplicationJson(responseAsyncResult.cause().getMessage())));
        }
        asyncResultHandler.handle(responseAsyncResult);
      });
  }

  public void updateProfilePictureInDbStorage (String profileId, byte[] requestedBytesArray,
                                                 Handler<AsyncResult<Response>> asyncResultHandler, Map<String, String> okapiHeaders, String encryptionKey) {
    try {
      byte[] encryptedData = encryptAES(requestedBytesArray, encryptionKey);
      byte[] hmac = calculateHmac(encryptedData, encryptionKey);
      JsonObject profilePictureDetails = createProfilePictureDetails(bytesToMegabytes(encryptedData.length));
      postgresClient
        .execute(createUpdateQuery(okapiHeaders), Tuple.of(encryptedData, hmac, profilePictureDetails, profileId))
        .compose(rows -> {
          if (rows.rowCount() != 0) {
            logger.info("updateProfilePictureInDbStorage:: Updated profile picture with id {}", profileId);
            return succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond200WithApplicationJson(new ProfilePicture().withId(UUID.fromString(profileId))));
          } else {
            logger.error("updateProfilePictureInDbStorage:: Can not find profile picture with id {}", profileId);
            return succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("Existing profile picture is not found"));
          }
        })
        .map(Response.class::cast)
        .onComplete(reply -> {
          if (reply.cause() != null) {
            logger.error("updateProfilePictureInDbStorage:: Can not update profile picture with id {}", profileId);
            asyncResultHandler.handle(
              succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond500WithApplicationJson(reply.cause().getMessage())));
          }
          asyncResultHandler.handle(reply);
        });
    } catch (Exception e) {
      logger.error("updateProfilePictureInDbStorage:: Error encrypting profile picture data: {}", e.getMessage());
      asyncResultHandler.handle(
        succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond500WithApplicationJson("Error encrypting profile picture data")));
    }
  }

  public Future<Config> getProfilePictureConfig(Map<String, String> okapiHeaders) {
    logger.info("getProfilePictureConfig:: Getting profile picture configuration...");
    return postgresClient
      .execute(createSelectQuery(okapiHeaders, GET_CONFIGURATION_SQL, TABLE_NAME_CONFIG))
      .compose(this::mapResultSetToConfig);
  }

  public Future<Config> getProfilePictureSetting() {
    logger.info("getProfilePictureSetting:: Getting profile picture config setting...");
    return postgresClient.withConn(conn -> settingsService.getProfilePictureConfigSetting(conn)
        .recover(err -> tryToFindAndMigrateSettingsFromConfigurationRepo(err, conn))
        .onFailure(err -> logger.warn("Failed to find Profile Picture config setting: {} {}",
          err.getClass().getSimpleName(), err.getMessage()))
      );
  }

  public Future<Void> cleanUp(Map<String, String> okapiHeaders) {
    logger.info("cleanUp:: Cleaning profile ids..");
    Promise<Void> resultPromise = Promise.promise();
    getProfilePictureSetting()
      .onSuccess(config -> {
        if (Objects.nonNull(config) && Boolean.FALSE.equals(config.getEnabled())) {
          logger.info("getProfileIdsAsync:: Profile picture feature is disabled");
          resultPromise.complete();
        } else if (Objects.nonNull(config) && Boolean.TRUE.equals(config.getEnabledObjectStorage())) {
          logger.info("cleanUp:: Cleaning profile picture from Object storage");
          removeUnusedProfileIdsFromObjectStorageAsync(okapiHeaders);
          resultPromise.complete();
        } else {
          logger.info("cleanUp:: Cleaning profile picture from db storage");
          removeUnusedProfileIdsFromDBStorageAsync(okapiHeaders)
            .onSuccess(v -> resultPromise.complete())
            .onFailure(resultPromise::fail);
        }
      })
      .onFailure(resultPromise::fail);
    return resultPromise.future();
  }

  public Future<Void> removeUnusedProfileIdsFromDBStorageAsync(Map<String, String> okapiHeaders) {
    logger.info("removeUnusedProfileIdsFromDBAsync:: Removing unused profile ids from DB..");
    Promise<Void> resultPromise = Promise.promise();

    postgresClient.execute(createDeleteQuery(okapiHeaders, DELETE_UNUSED_PROFILE_IDS, TABLE_NAME_PROFILE_PICTURE))
      .onComplete(ar -> {
        if (ar.succeeded()) {
          logger.info("removeUnusedProfileIdsFromDBAsync:: Removed unused profilePictures successfully");
          resultPromise.complete();
        } else {
          logger.error("removeUnusedProfileIdsFromDBAsync:: Failed to execute query", ar.cause());
          resultPromise.fail(ar.cause());
        }
      });

    return resultPromise.future();
  }

  public void removeUnusedProfileIdsFromObjectStorageAsync(Map<String, String> okapiHeaders) {
    logger.info("getUsersProfileLinkIdsAsync:: Getting users profile linked ids..");
    CompletableFuture<Void> resultFuture = new CompletableFuture<>();
    CompletableFuture<List<String>> userProfileIdsFuture = getUsersProfileLinkIdsAsync(okapiHeaders);
    final String TENANT_FOLDER = okapiHeaders.get(OkapiConnectionParams.OKAPI_TENANT_HEADER) + "/";
    userProfileIdsFuture.thenAccept(userProfileIds -> {
      try {
        String startAfter = null;
        FolioS3Client client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
        do {
          List<String> pageOfObjectStorageIds = getObjectStorageIdsPage(TENANT_FOLDER, startAfter, client);
          filterUnusedIds(pageOfObjectStorageIds,TENANT_FOLDER, client, userProfileIds);

          // If the size of the current page is less than maxKeys, it means there are no more objects
          if (pageOfObjectStorageIds.size() < MAX_IDS_COUNT) {
            break;
          }

          startAfter = pageOfObjectStorageIds.get(pageOfObjectStorageIds.size() - 1);
        } while (true);

        resultFuture.complete(null);
      } catch (Exception e) {
        logger.error("Error processing Object Storage IDs", e);
        resultFuture.completeExceptionally(e);
      }
    });
  }

  public CompletableFuture<List<String>> getUsersProfileLinkIdsAsync(Map<String, String> okapiHeaders) {
    logger.info("getUsersProfileLinkIdsAsync:: Getting users profile linked ids..");

    CompletableFuture<List<String>> resultFuture = new CompletableFuture<>();

    postgresClient.execute(createSelectQuery(okapiHeaders, SELECT_USERS_PROFILE_LINK_ID, TABLE_NAME_USERS))
      .onComplete(ar -> {
        if (ar.succeeded()) {
          List<String> ids = new ArrayList<>();
          for (Row row : ar.result()) {
            ids.add(row.getString(PROFILE_LINK_IDS));
          }
          logger.info("Query result: {}", ids);
          resultFuture.complete(ids);
        } else {
          logger.error("Failed to execute query", ar.cause());
          resultFuture.completeExceptionally(ar.cause());
        }
      });
    return resultFuture;
  }

  private Future<Config> mapResultSetToConfig(RowSet<Row> rows) {
    Promise<Config> promise = Promise.promise();
    Config config = new Config();

    if (rows.rowCount() != 0) {
      Row row = rows.iterator().next();
      JsonObject json = row.get(JsonObject.class, JSONB);
      if (json != null) {
        config
          .withId(String.valueOf(row.getUUID(ID)))
          .withConfigName(row.getString(CONFIG_NAME))
          .withEnabled(json.getBoolean(ENABLED))
          .withEnabledObjectStorage(json.getBoolean(ENABLED_OBJECT_STORAGE))
          .withEncryptionKey(json.getString(ENCRYPTION_KEY))
          .withMaxFileSize(json.getDouble(MAX_FILE_SIZE));
      }
      promise.complete(config);
    } else {
      promise.fail("No rows found in the result set");
    }
    return promise.future();
  }

  private Future<Config> tryToFindAndMigrateSettingsFromConfigurationRepo(Throwable err, Conn conn) {
    logger.info("tryToFindAndMigrateSettingsFromConfigurationRepo:: setting is absent: {} {}",
      err.getClass().getSimpleName(), err.getMessage());

    var searchCriteria = new Criteria().addField("'configName'").setOperation("=").setVal(PROFILE_PICTURE_SETTING_KEY);
    var criterion = new Criterion().addCriterion(searchCriteria).setLimit(new Limit(1));

    return conn.get(TABLE_NAME_CONFIG, Config.class, criterion, true)
      .compose(results -> {
        if (results.getResults().isEmpty()) {
          logger.info("tryToFindAndMigrateSettingsFromConfigurationRepo:: no configuration found to migrate");
          return Future.failedFuture("No Profile Picture configuration is found");
        } else {
          logger.info("tryToFindAndMigrateSettingsFromConfigurationRepo:: Profile Picture configuration is found");
          return Future.succeededFuture(results.getResults().getFirst());
        }
      })
      .compose(profilePictureConfig -> settingsService.createProfilePictureConfigSetting(conn, profilePictureConfig)
        .map(newProfilePictureConfig -> Pair.of(profilePictureConfig.getId(), newProfilePictureConfig)))
      .compose(profilePictureConfigPair -> deleteDeprecatedProfilePictureConfig(conn, profilePictureConfigPair))
      .onSuccess(result ->
        logger.debug("tryToFindAndMigrateSettingsFromConfigurationRepo:: result: {}", () -> result));
  }

  private Future<Config> deleteDeprecatedProfilePictureConfig(Conn conn, Pair<String, Config> configPair) {
    var deprecatedConfigId = configPair.getLeft();
    var newConfig = configPair.getRight();
    logger.info("deleteDeprecatedProfilePictureConfig:: Deleting deprecated profile picture configuration with id {}", deprecatedConfigId);
    return conn.delete(TABLE_NAME_CONFIG, deprecatedConfigId)
      .map(result -> result.rowCount() > 0)
      .onSuccess(result ->
        logger.info("deleteDeprecatedProfilePictureConfig:: Deleted deprecated profile picture configuration: {}", result))
      .map(result -> newConfig);
  }

  private List<String> getObjectStorageIdsPage(String path, String startAfter, FolioS3Client client) {
    try {
      return client.list(path, MAX_IDS_COUNT, startAfter);
    } catch (Exception e) {
      throw new S3ClientException("Error getting list of objects : {} ", e);
    }
  }

  private void filterUnusedIds(List<String> objectStorageIds, String folder, FolioS3Client client, List<String> userProfileIds) {
    List<String> unusedIds = objectStorageIds.stream()
      .filter(objectStorageId -> !userProfileIds.contains(objectStorageId.substring(folder.length())))
      .toList();

    client.remove(unusedIds.toArray(new String[0]));
  }

  private ProfilePicture mapResultSetToProfilePicture(Row row, String encryptionKey) {
    ProfilePicture profilePicture = new ProfilePicture();
    profilePicture.withId(UUID.fromString(row.getValue(ID).toString()));
    byte[] encryptedData = row.getBuffer(BLOB).getBytes();

    try {
      byte[] decryptedData = decryptAES(encryptedData, encryptionKey);
      profilePicture.withProfilePictureBlob(Base64.getEncoder().encodeToString(decryptedData));
    } catch (Exception e) {
      logger.error("Error decrypting profile picture data: {}", e.getMessage());
      throw new IllegalStateException("Error decrypting profile picture data");
    }

    return profilePicture;
  }

  public static String createInsertQuery(Map<String, String> okapiHeaders) {
    return String.format(SAVE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }

  public static String createSelectQuery(Map<String, String> okapiHeaders, String sql, String tableName) {
    return String.format(sql, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), tableName);
  }

  private static String createDeleteQuery(Map<String, String> okapiHeaders, String sql, String tableName) {
    return String.format(sql, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), tableName, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_USERS);
  }

  private static String createUpdateQuery(Map<String, String> okapiHeaders) {
    return String.format(UPDATE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }
}
