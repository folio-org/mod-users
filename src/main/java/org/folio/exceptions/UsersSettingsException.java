package org.folio.exceptions;

import static javax.ws.rs.core.HttpHeaders.CONTENT_TYPE;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;

import javax.ws.rs.core.Response;
import java.util.List;
import lombok.Getter;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;

/**
 * Exception thrown when users settings operations fail.
 */
public class UsersSettingsException extends RuntimeException {

  private final int statusCode;
  /**
   * -- GETTER --
   *  Returns the error payload associated with this exception.
   *
   * @return the {@link Error} object containing error details
   */
  @Getter
  private final transient Error error;

  /**
   * Creates an exception for a setting not found by ID.
   *
   * @param id the ID of the setting that was not found
   * @return a UsersSettingsException with NOT_FOUND status
   */
  public static UsersSettingsException notFoundById(String id) {
    var err = new Error()
      .withMessage("Setting entity not found by id: " + id)
      .withCode("not_found_error")
      .withParameters(List.of(new Parameter().withKey("id").withValue(id)));
    return new UsersSettingsException(err, Response.Status.NOT_FOUND);
  }

  /**
   * Constructs a UsersSettingsException without capturing a stack trace.
   *
   * <p>The stack trace is disabled to reduce allocation overhead for expected control-flow errors.
   *
   * @param error      the error payload to return to the client
   * @param statusCode the HTTP status to use for the response
   */
  public UsersSettingsException(Error error, Response.Status statusCode) {
    super(error.getMessage(), null, true, false);
    this.error = error;
    this.statusCode = statusCode.getStatusCode();
  }

  /**
   * Constructs a UsersSettingsException without capturing a stack trace.
   *
   * <p>The stack trace is disabled to reduce allocation overhead for expected control-flow errors.
   *
   * @param error      the error payload to return to the client
   * @param statusCode the HTTP status to use for the response
   */
  public UsersSettingsException(Error error, int statusCode) {
    super(error.getMessage(), null, true, false);
    this.error = error;
    this.statusCode = statusCode;
  }

  /**
   * Constructs a UsersSettingsException with a cause, without capturing a stack trace.
   *
   * <p>The stack trace is disabled to reduce allocation overhead for expected control-flow errors.
   *
   * @param error      the error payload to return to the client
   * @param statusCode the HTTP status to use for the response
   * @param cause      the cause of this exception
   */
  public UsersSettingsException(Error error, int statusCode, Throwable cause) {
    super(error.getMessage(), cause, true, false);
    this.error = error;
    this.statusCode = statusCode;
  }

  /**
   * Builds a JAX-RS {@link Response} from this exception using the configured error payload and status.
   *
   * @return a JAX-RS Response with JSON error entity and appropriate status
   */
  public Response buildErrorResponse() {
    return Response.status(statusCode)
      .entity(new Errors().withErrors(List.of(error)))
      .header(CONTENT_TYPE, APPLICATION_JSON)
      .build();
  }
}
