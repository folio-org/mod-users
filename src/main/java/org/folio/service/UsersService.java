package org.folio.service;

import io.vertx.core.Future;
import io.vertx.ext.web.handler.HttpException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Personal;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.persist.Conn;

import javax.ws.rs.core.Response;

import static org.folio.support.UsersApiConstants.TABLE_NAME_USERS;

public class UsersService {

  private static final Logger logger = LogManager.getLogger(UsersService.class);

  public Future<User> getUserByIdForUpdate(Conn conn, String userId) {
    return conn.getByIdForUpdate(TABLE_NAME_USERS, userId, User.class)
      .onFailure(t -> logger.error("getUserByIdForUpdate failed, userId={}", userId, t));
  }

  public Future<User> updateUser(Conn conn, User user) {
    return conn.update(TABLE_NAME_USERS, user, user.getId())
      .compose(rowSet -> {
        if (rowSet.rowCount() == 0) {
          String errorMsg = String.format("User with id %s was not found", user.getId());
          return Future.failedFuture(new HttpException(Response.Status.NOT_FOUND.getStatusCode(), errorMsg));
        }
        return Future.succeededFuture(user);
      })
      .onSuccess(x -> logger.info("updateUser complete, userId={}", user.getId()))
      .onFailure(e -> logger.error("updateUser failed, userId={}", user.getId(), e));
  }

  public static User getConsortiumUserDto(User user) {
    User userDto = new User()
      .withId(user.getId())
      .withUsername(user.getUsername())
      .withActive(user.getActive())
      .withType(user.getType())
      .withBarcode(user.getBarcode())
      .withExternalSystemId(user.getExternalSystemId());

    if (user.getPersonal() != null) {
      userDto.withPersonal(new Personal()
        .withFirstName(user.getPersonal().getFirstName())
        .withLastName(user.getPersonal().getLastName())
        .withEmail(user.getPersonal().getEmail())
        .withPhone(user.getPersonal().getPhone())
        .withMobilePhone(user.getPersonal().getMobilePhone())
      );
    }
    return userDto;
  }
}
