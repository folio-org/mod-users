package org.folio.rest.impl;

import static org.apache.http.HttpStatus.SC_CREATED;
import static org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY;
import static org.folio.test.util.TestUtil.readFile;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.jaxrs.model.PatronBlockLimit;
import org.folio.rest.jaxrs.model.PatronBlockLimits;
import org.folio.test.util.TestBase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.restassured.http.Header;
import io.restassured.response.ExtractableResponse;
import io.restassured.response.Response;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class PatronBlockLimitsAPITest extends TestBase {

  private static final String PATRON_BLOCK_LIMITS_URL = "/patron-block-limits/";
  private static final String PATRON_BLOCK_LIMITS = "patron-block-limits";
  private static final Header USER_ID = new Header(XOkapiHeaders.USER_ID, "111111111");

  @After
  public void tearDown() {
    PatronBlockLimits response = getWithOk(PATRON_BLOCK_LIMITS_URL)
      .as(PatronBlockLimits.class);

    List<PatronBlockLimit> patronBlockLimits = response.getPatronBlockLimits();

    if (!patronBlockLimits.isEmpty()) {
      patronBlockLimits.forEach(entity -> deleteWithNoContent(PATRON_BLOCK_LIMITS_URL + entity.getId()));
    }
  }

 @Test
  public void cannotCreatePatronBlockLimitWithInvalidIntegerLimit()
   throws IOException, URISyntaxException {

    String patronBlockLimit = readFile(PATRON_BLOCK_LIMITS
      + "/limit_max_number_of_lost_items_invalid_limit.json");
    PatronBlockLimit actualLimit = postWithStatus(PATRON_BLOCK_LIMITS_URL,
      patronBlockLimit, SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockLimit.class);

    List<Map<String, Object>> errors =  (List<Map<String, Object>>) actualLimit.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("Must be blank or a number from 0 to 999999"));
  }

  @Test
  public void cannotCreatePatronBlockLimitWithInvalidDoubleLimit()
    throws IOException, URISyntaxException {

    String patronBlockLimit = readFile(PATRON_BLOCK_LIMITS
      + "/limit_max_outstanding_feefine_balance_invalid_limit.json");
    PatronBlockLimit actualLimit = postWithStatus(PATRON_BLOCK_LIMITS_URL,
      patronBlockLimit, SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockLimit.class);

    List<Map<String, Object>> errors =  (List<Map<String, Object>>) actualLimit.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("A maximum balance of 0 will result in all patrons in this group " +
      "being blocked; to skip this limit, leave value set to blank"));
  }

  @Test
  public void canGetAllPatronBlockLimits() throws IOException, URISyntaxException {

    postAllLimits();
    PatronBlockLimits response = getWithOk(PATRON_BLOCK_LIMITS_URL)
      .as(PatronBlockLimits.class);
    assertThat(response.getTotalRecords(), is(6));
  }

  @Test
  public void putPatronBlockLimitsByPatronBlockLimitId() {

  }

  @Test
  public void getPatronBlockLimitsByPatronBlockLimitId() {

  }

  @Test
  public void deletePatronBlockLimitsByPatronBlockLimitId() {

  }

  private void postAllLimits() throws IOException, URISyntaxException {

    List<String> limits = new ArrayList<>();
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_max_number_of_items_charged_out.json"));
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_max_number_of_lost_items.json"));
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_max_number_of_overdue_items.json"));
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_max_number_of_overdue_recalls.json"));
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_max_outstanding_feefine_balance.json"));
    limits.add(readFile(PATRON_BLOCK_LIMITS + "/limit_recall_overdue_by_max_number_of_days.json"));

    limits.forEach(limit -> postWithStatus(PATRON_BLOCK_LIMITS_URL, limit, SC_CREATED, USER_ID)
      .as(PatronBlockLimit.class));
  }
}
