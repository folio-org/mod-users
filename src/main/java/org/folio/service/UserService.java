package org.folio.service;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.handler.HttpException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Conn;

import javax.ws.rs.core.Response;

import static org.folio.rest.impl.UsersAPI.TABLE_NAME_USERS;

public class UserService {

  private static final Logger logger = LogManager.getLogger(UserService.class);

  public Future<User> getUserById(Conn conn, String userId) {
    Promise<User> promise = Promise.promise();

    conn.getById(TABLE_NAME_USERS, userId, User.class)
      .onComplete(ar -> {
        if (ar.failed()) {
          logger.error("getUserById(conn, user) failed, userId={}", userId, ar.cause());
        } else {
          promise.complete(ar.result());
        }
      });
    return promise.future();
  }

  public Future<User> updateUser(Conn conn, User user) {
    Promise<User> promise = Promise.promise();

    conn.update(TABLE_NAME_USERS, user, user.getId())
      .onComplete(ar -> {
        if (ar.failed()) {
          logger.error("updateUser(conn, user) failed, user={}",
            JsonObject.mapFrom(user).encodePrettily(), ar.cause());
          promise.fail(new HttpException(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), ar.cause()));
        } else {
          if (ar.result().rowCount() == 0) {
            logger.error("updateUser(conn, user): no line was updated");
            promise.fail(new HttpException(Response.Status.NOT_FOUND.getStatusCode(), Response.Status.NOT_FOUND.getReasonPhrase()));
          } else {
            logger.info("updateUser(conn, user) complete, userId={}", user.getId());
            promise.complete(user);
          }
        }
      });
    return promise.future();
  }

  public static User getConsortiumUserDto(User user) {
    return new User()
      .withUsername(user.getUsername())
      .withPersonal(new Personal()
        .withEmail(user.getPersonal().getEmail())
        .withPhone(user.getPersonal().getPhone())
        .withMobilePhone(user.getPersonal().getMobilePhone())
      );
  }
}
