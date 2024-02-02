package org.folio.service.storage;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.tools.utils.TenantTool;

import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.util.Base64;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;
import static org.folio.support.ProfilePictureHelper.calculateHmac;
import static org.folio.support.ProfilePictureHelper.decryptAES;
import static org.folio.support.ProfilePictureHelper.encryptAES;
import static org.folio.support.ProfilePictureHelper.verifyHmac;
import static org.folio.support.UsersApiConstants.BLOB;
import static org.folio.support.UsersApiConstants.CHECKSUM;
import static org.folio.support.UsersApiConstants.CONFIG_NAME;
import static org.folio.support.UsersApiConstants.ENABLED;
import static org.folio.support.UsersApiConstants.ENABLED_OBJECT_STORAGE;
import static org.folio.support.UsersApiConstants.ENCRYPTION_KEY;
import static org.folio.support.UsersApiConstants.GET_CONFIGURATION_SQL;
import static org.folio.support.UsersApiConstants.GET_PROFILE_PICTURE_SQL;
import static org.folio.support.UsersApiConstants.ID;
import static org.folio.support.UsersApiConstants.JSONB;
import static org.folio.support.UsersApiConstants.PROFILE_PICTURE_FOLDER;
import static org.folio.support.UsersApiConstants.SAVE_PROFILE_PICTURE_SQL;
import static org.folio.support.UsersApiConstants.TABLE_NAME_CONFIG;
import static org.folio.support.UsersApiConstants.TABLE_NAME_PROFILE_PICTURE;
import static org.folio.support.UsersApiConstants.UPDATE_PROFILE_PICTURE_SQL;

public class ProfilePictureStorage {
  private final FolioS3ClientFactory folioS3ClientFactory = new FolioS3ClientFactory();
  private String path;
  private static final String SEPARATOR = "/";
  private static final Logger logger = LogManager.getLogger(ProfilePictureStorage.class);

  public void storeProfilePictureInObjectStorage(byte[] fileBytes, Map<String, String> okapiHeaders, String profileId,
                        Handler<AsyncResult<Response>> asyncResultHandler) {
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
      try {
        if (StringUtils.isNotEmpty(profileId)) {
          path = PROFILE_PICTURE_FOLDER + SEPARATOR + profileId;
          client.getSize(path);
          logger.info("storeProfilePictureInObjectStorage:: Updating file in to folder {}", PROFILE_PICTURE_FOLDER);
          client.write(path, new ByteArrayInputStream(fileBytes), fileBytes.length);
          asyncResultHandler.handle(succeededFuture(Users.PutUsersProfilePictureByProfileIdResponse.respond200WithApplicationJson(new ProfilePicture().withId(UUID.fromString(path.substring(path.lastIndexOf("/") + 1))))));
          }
        else if (Objects.isNull(path)) {
          logger.debug("storeProfilePictureInObjectStorage:: Creating folder with name {}", PROFILE_PICTURE_FOLDER);
          path = PROFILE_PICTURE_FOLDER + SEPARATOR + UUID.randomUUID();
          logger.info("storeProfilePictureInObjectStorage:: Writing file in to folder {}", PROFILE_PICTURE_FOLDER);
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
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
    try {
      path = PROFILE_PICTURE_FOLDER + SEPARATOR + profileId;
      logger.info("storeProfilePictureInObjectStorage:: Removing file {} from folder {}", profileId, PROFILE_PICTURE_FOLDER);
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
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
    try {
      logger.info("getProfilePictureFromObjectStorage:: Getting profile picture from object storage with id {}", fileName);
      path = PROFILE_PICTURE_FOLDER + SEPARATOR + fileName;
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
                                             Handler<AsyncResult<Response>> asyncResultHandler, String encryptionKey, Context vertxContext) {
    try {
      UUID profileId = UUID.randomUUID();
      byte[] encryptedData = encryptAES(requestBytesArray, encryptionKey);
      byte[] hmac = calculateHmac(encryptedData, encryptionKey);

      Tuple params = Tuple.of(profileId, encryptedData, hmac);
      PgUtil.postgresClient(vertxContext, okapiHeaders)
        .execute(createInsertQuery(okapiHeaders), params)
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

  public void getProfilePictureFromDbStorage(String profileId, Handler<AsyncResult<Response>> asyncResultHandler,
                                             Map<String, String> okapiHeaders, String encryptionKey, Context vertxContext) {
    logger.info("getProfilePictureFromDbStorage:: Getting profile picture from db storage with id {}", profileId);
    Tuple params = Tuple.of(profileId);
    PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createSelectQuery(okapiHeaders, GET_PROFILE_PICTURE_SQL, TABLE_NAME_PROFILE_PICTURE), params)
      .compose(rows -> {
        if (rows.rowCount() != 0) {
          Row row = rows.iterator().next();
          byte[] encryptedData = row.getBuffer(BLOB).getBytes();
          byte[] storedHmac = row.getBuffer(CHECKSUM).getBytes();
          try {
            if (!verifyHmac(encryptedData, storedHmac, encryptionKey)) {
              return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond400WithApplicationJson("Data integrity check failed"));
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
                                                 Handler<AsyncResult<Response>> asyncResultHandler, Map<String, String> okapiHeaders, String encryptionKey, Context vertxContext) {
    try {
      byte[] encryptedData = encryptAES(requestedBytesArray, encryptionKey);
      byte[] hmac = calculateHmac(encryptedData, encryptionKey);
      PgUtil.postgresClient(vertxContext, okapiHeaders)
        .execute(createUpdateQuery(okapiHeaders), Tuple.of(encryptedData, hmac, profileId))
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
          .withEncryptionKey(json.getString(ENCRYPTION_KEY));
      }
      promise.complete(config);
    } else {
      promise.fail("No rows found in the result set");
    }
    return promise.future();
  }

  public Future<Config> getProfilePictureConfig(Map<String, String> okapiHeaders, Context vertxContext) {
    logger.info("getProfilePictureConfig:: Getting profile picture configuration...");
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createSelectQuery(okapiHeaders, GET_CONFIGURATION_SQL, TABLE_NAME_CONFIG))
      .compose(this::mapResultSetToConfig);
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

  private static String createUpdateQuery(Map<String, String> okapiHeaders) {
    return String.format(UPDATE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }
}
