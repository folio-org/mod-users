package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static javax.ws.rs.core.Response.Status.NOT_FOUND;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import javax.ws.rs.core.Response;
import java.lang.reflect.Method;
import org.folio.exceptions.UsersSettingsException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.support.tags.UnitTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@UnitTest
class UserSettingsAPITest {

  private UserSettingsAPI api;
  private Method handleError;

  @BeforeEach
  void setUp() throws Exception {
    api = new UserSettingsAPI();
    handleError = UserSettingsAPI.class.getDeclaredMethod("handleError", Throwable.class);
    handleError.setAccessible(true);
  }

  @Test
  void handleError_shouldDelegateToUsersSettingsException_whenThrowableIsUsersSettingsException() throws Exception {
    var exception = UsersSettingsException.notFoundById("some-id");
    var response = (Response) handleError.invoke(api, exception);

    assertThat(response.getStatus(), is(NOT_FOUND.getStatusCode()));
    assertThat(response.getHeaderString("Content-Type"), is(APPLICATION_JSON));
    var errors = (Errors) response.getEntity();
    assertThat(errors.getErrors().getFirst().getCode(), is("not_found_error"));
  }

  @Test
  void handleError_shouldReturnInternalServerError_whenThrowableIsUnexpectedException() throws Exception {
    var exception = new RuntimeException("something went wrong");
    var response = (Response) handleError.invoke(api, exception);

    assertThat(response.getStatus(), is(INTERNAL_SERVER_ERROR.getStatusCode()));
    assertThat(response.getHeaderString("Content-Type"), is(APPLICATION_JSON));
    var error = (Error) response.getEntity();
    assertThat(error.getCode(), is("settings_error"));
    assertThat(error.getType(), is("RuntimeException"));
    assertThat(error.getMessage(), is("Unexpected error occurred: something went wrong"));
  }
}
