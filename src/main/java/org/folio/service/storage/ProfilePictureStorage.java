package org.folio.service.storage;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.sqlclient.Tuple;
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

public class ProfilePictureStorage {
  public static final String TABLE_NAME_PROFILE_PICTURE = "profile_picture";
  private static final String PROFILE_PICTURE_FOLDER = "Profile-Pictures";
  private static final String SAVE_PROFILE_PICTURE_SQL = "INSERT INTO %s.%s (id, profile_picture_blob) VALUES ($1, $2)";
  @Autowired
  private final FolioS3ClientFactory folioS3ClientFactory = new FolioS3ClientFactory();
  private String path;

  public void storeProfilePictureInObjectStorage(byte[] fileBytes,
                        Handler<AsyncResult<Response>> asyncResultHandler) {
    var client = folioS3ClientFactory.getFolioS3Client();
      try {
        if (Objects.isNull(path)) {
          path = PROFILE_PICTURE_FOLDER + "/" + UUID.randomUUID();
        }
        client.write(path, new ByteArrayInputStream(fileBytes), fileBytes.length);
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(UUID.fromString(path.substring(path.lastIndexOf("/") + 1))))));
      } catch (Exception e) {
        e.printStackTrace();
        asyncResultHandler.handle(succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(String.format("Error storing file [%s]", e.getMessage()))));
      }
  }

  public void storeProfilePictureInDbStorage(byte[] requestBytesArray, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    UUID id = UUID.randomUUID();
    Tuple params = Tuple.of(id, requestBytesArray);
    PgUtil.postgresClient(vertxContext, okapiHeaders)
      .execute(createInsertQuery(okapiHeaders), params)
      .map(rows -> Users.PostUsersProfilePictureResponse.respond201WithApplicationJson(new ProfilePicture().withId(id)))
      .map(Response.class::cast)
      .onComplete(reply -> {
        if (reply.cause() != null) {
          asyncResultHandler.handle(
            succeededFuture(Users.PostUsersProfilePictureResponse.respond500WithApplicationJson(reply.cause().getMessage())));
        }
        asyncResultHandler.handle(reply);
      });
  }

  public void getProfilePictureFromObjectStorage(String fileName,
                                                 Handler<AsyncResult<Response>> asyncResultHandler) {
      var client = folioS3ClientFactory.getFolioS3Client();
      try {
        path = PROFILE_PICTURE_FOLDER + "/" + fileName;
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

  private static String createInsertQuery(Map<String, String> okapiHeaders) {
    return String.format(SAVE_PROFILE_PICTURE_SQL, convertToPsqlStandard(TenantTool.tenantId(okapiHeaders)), TABLE_NAME_PROFILE_PICTURE);
  }
}
