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
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.ProfilePicture;
import org.folio.rest.jaxrs.resource.Users;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.tools.utils.TenantTool;
import org.springframework.beans.factory.annotation.Autowired;

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
import static org.folio.support.UsersApiConstants.*;

public class ProfilePictureStorage {
  @Autowired
  private final FolioS3ClientFactory folioS3ClientFactory = new FolioS3ClientFactory();
  private String path;
  private final static String SEPARATOR = "/";

  public void storeProfilePictureInObjectStorage(byte[] fileBytes, Map<String, String> okapiHeaders,
                        Handler<AsyncResult<Response>> asyncResultHandler) {
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
      try {
        if (Objects.isNull(path)) {
          path = PROFILE_PICTURE_FOLDER + SEPARATOR + UUID.randomUUID();
        }
        client.write(path, new ByteArrayInputStream(fileBytes), fileBytes.length);
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(UUID.fromString(path.substring(path.lastIndexOf("/") + 1))))));
      } catch (Exception e) {
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(String.format("Error storing file [%s]", e.getCause().getMessage()))));
      }
  }

  public void storeProfilePictureInDbStorage(byte[] requestBytesArray, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    UUID profileId = UUID.randomUUID();
    Tuple params = Tuple.of(profileId, requestBytesArray);
    PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createInsertQuery(okapiHeaders), params)
      .map(rows -> Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(profileId)))
      .map(Response.class::cast)
      .onComplete(reply -> {
        if (reply.cause() != null) {
          asyncResultHandler.handle(
            succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(reply.cause().getMessage())));
        }
        asyncResultHandler.handle(reply);
      });
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
          .withEnabledObjectStorage(json.getBoolean(ENABLED_OBJECT_STORAGE));
      }
      promise.complete(config);
    } else {
      promise.fail("No rows found in the result set");
    }
    return promise.future();
  }

  public Future<Config> getProfilePictureConfig(Map<String, String> okapiHeaders, Context vertxContext) {
    return PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createSelectQuery(okapiHeaders, GET_CONFIGURATION_SQL, TABLE_NAME_CONFIG))
      .compose(this::mapResultSetToConfig);
  }

  public void getProfilePictureFromObjectStorage(String fileName,
                                                 Handler<AsyncResult<Response>> asyncResultHandler, Map<String, String> okapiHeaders) {
    var client = folioS3ClientFactory.getFolioS3Client(okapiHeaders);
    try {
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
      asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(String.format("Error storing file [%s]", e.getMessage()))));
    }
  }

  public void getProfilePictureFromDbStorage (String profileId,
                                              Handler<AsyncResult<Response>> asyncResultHandler, Map<String, String> okapiHeaders, Context vertxContext) {
    Tuple params = Tuple.of(profileId);
    PgUtil.postgresClient(vertxContext, okapiHeaders).execute(createSelectQuery(okapiHeaders, GET_PROFILE_PICTURE_SQL, TABLE_NAME_PROFILE_PICTURE), params)
      .compose(rows -> {
        if (rows.rowCount() != 0) {
          return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond200WithApplicationJson(mapResultSetToProfilePicture(rows)));
        } else {
          return succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse.respond404WithTextPlain("No profile picture found for id " + profileId));
        }
      })
      .map(Response.class::cast)
      .onComplete(responseAsyncResult -> {
        if (responseAsyncResult.cause() != null) {
          asyncResultHandler.handle(
            succeededFuture(Users.GetUsersProfilePictureByProfileIdResponse
              .respond400WithApplicationJson(responseAsyncResult.cause().getMessage())));
        }
        asyncResultHandler.handle(responseAsyncResult);
      });
  }

  private ProfilePicture mapResultSetToProfilePicture(RowSet<Row> resultSet) {
    ProfilePicture profilePicture = new ProfilePicture();
    for (Row row : resultSet) {
      profilePicture
        .withId(UUID.fromString(row.getValue(ID).toString()))
        .withProfilePictureBlob(Base64.getEncoder().encodeToString(row.getBuffer(BLOB).getBytes()));
    }
    return profilePicture;
  }

  public static String createInsertQuery(Map<String, String> okapiHeaders) {
    return String.format(SAVE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }

  public static String createSelectQuery(Map<String, String> okapiHeaders, String sql, String tableName) {
    return String.format(sql, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), tableName);
  }
}
