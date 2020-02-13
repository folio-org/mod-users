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
import org.folio.rest.jaxrs.model.PatronBlockCondition;
import org.folio.rest.jaxrs.model.PatronBlockConditions;
import org.folio.test.util.TestBase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.restassured.http.Header;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class PatronBlockConditionsAPITest extends TestBase {

  private static final String MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID = "72b67965-5b73-4840-bc0b-be8f3f6e047e";
  private static final String MAX_NUMBER_OF_ITEMS_CHARGED_OUT = "3d7c52dc-c732-4223-8bf8-e5917801386f";
  private static final String MAX_NUMBER_OF_OVERDUE_ITEMS = "584fbd4f-6a34-4730-a6ca-73a6a6a9d845";
  private static final String MAX_NUMBER_OF_OVERDUE_RECALLS = "e5b45031-a202-4abb-917b-e1df9346fe2c";
  private static final String MAX_OUTSTANDING_FEEFINE_BALANCE = "cf7a0d5f-a327-4ca1-aa9e-dc55ec006b8a";
  private static final String RECALL_OVERDUE_BY_MAX_NUMBER = "08530ac4-07f2-48e6-9dda-a97bc2bf7053";
  private static final Header USER_ID = new Header(XOkapiHeaders.USER_ID, "111111111");
  private static final String PATRON_BLOCK_CONDITIONS_URL = "/patron-block-conditions/";
  private static final String PATRON_BLOCK_CONDITIONS = "patron-block-conditions";

  @Before
  public void setUp() throws IOException, URISyntaxException {
    List<String> conditions = new ArrayList<>();
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/max_number_of_items_charged_out.json"));
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/max_number_of_lost_items.json"));
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/max_number_of_overdue_items.json"));
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/max_number_of_overdue_recalls.json"));
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/max_outstanding_feefine_balance.json"));
    conditions.add(readFile(PATRON_BLOCK_CONDITIONS + "/recall_overdue_by_max_number_of_days.json"));

    conditions.forEach(condition -> postWithStatus(PATRON_BLOCK_CONDITIONS_URL, condition, SC_CREATED, USER_ID)
      .as(PatronBlockCondition.class));
  }

  @After
  public void tearDown() {
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID);
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + MAX_NUMBER_OF_ITEMS_CHARGED_OUT);
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + MAX_NUMBER_OF_OVERDUE_ITEMS);
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + MAX_NUMBER_OF_OVERDUE_RECALLS);
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + MAX_OUTSTANDING_FEEFINE_BALANCE);
    deleteWithNoContent(PATRON_BLOCK_CONDITIONS_URL + RECALL_OVERDUE_BY_MAX_NUMBER);
  }

  @Test
  public void shouldReturnAllConditions() {
    PatronBlockConditions conditions = getWithOk(PATRON_BLOCK_CONDITIONS_URL)
      .as(PatronBlockConditions.class);
    assertThat(conditions.getTotalRecords(), equalTo(6));
  }

  @Test
  public void shouldReturnMaxNumberOfLostItemsCondition() {
    PatronBlockCondition patronBlockCondition = getWithOk(PATRON_BLOCK_CONDITIONS_URL
      + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID)
      .as(PatronBlockCondition.class);
    assertThat(patronBlockCondition.getName(), equalTo("Maximum number of lost items"));
  }

  @Test
  public void shouldUpdateMaxNumberOfLostItemsCondition()
    throws IOException, URISyntaxException {

    String updatedMaxNumberOfLostItemsCondition = readFile(PATRON_BLOCK_CONDITIONS
      +"/max_number_of_lost_items_updated.json");

    putWithNoContent(PATRON_BLOCK_CONDITIONS_URL
      + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID, updatedMaxNumberOfLostItemsCondition, USER_ID);
    PatronBlockCondition updatedCondition = getWithOk(PATRON_BLOCK_CONDITIONS_URL
      + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID).as(PatronBlockCondition.class);

    assertThat(updatedCondition.getBlockBorrowing(), equalTo(true));
    assertThat(updatedCondition.getBlockRenewals(), equalTo(true));
    assertThat(updatedCondition.getBlockRequests(), equalTo(true));
    assertThat(updatedCondition.getAdditionalProperties().get("message"),
      equalTo("Maximum number of lost items has been reached"));
  }

  @Test
  public void cannotPostMaxNumberOfLostItemWithNoMessage()
    throws IOException, URISyntaxException {

    String maxNumberOfLostItems = readFile(PATRON_BLOCK_CONDITIONS
      +"/max_number_of_lost_items_no_message.json");
    PatronBlockCondition response = postWithStatus(PATRON_BLOCK_CONDITIONS_URL, maxNumberOfLostItems,
      SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockCondition.class);

    List<Map<String, Object>> errors =  (List<Map<String, Object>>) response.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("Message to be displayed is a required field if one or more blocked actions selected"));
  }

  @Test
  public void cannotUpdateMaxNumberOfLostItemWithNoMessage()
    throws IOException, URISyntaxException {

    String maxNumberOfLostItems = readFile(PATRON_BLOCK_CONDITIONS
      +"/max_number_of_lost_items_no_message.json");
    PatronBlockCondition response = putWithStatus(PATRON_BLOCK_CONDITIONS_URL
        + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID, maxNumberOfLostItems, SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockCondition.class);
    List<Map<String, Object>> errors =  (List<Map<String, Object>>) response.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("Message to be displayed is a required field if one or more blocked actions selected"));
  }

  @Test
  public void cannotPostMaxNumberOfLostItemWithMessageAndNoFlagSetToTrue()
    throws IOException, URISyntaxException {

    String maxNumberOfLostItems = readFile(PATRON_BLOCK_CONDITIONS
      +"/max_number_of_lost_items_no_flat_set_to_true.json");
    PatronBlockCondition response = postWithStatus(PATRON_BLOCK_CONDITIONS_URL, maxNumberOfLostItems,
      SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockCondition.class);
    List<Map<String, Object>> errors =  (List<Map<String, Object>>) response.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("One or more blocked actions must be selected for message to be displayed to be used"));
  }

  @Test
  public void cannotUpdateMaxNumberOfLostItemWithMessageAndNoFlagSetToTrue()
    throws IOException, URISyntaxException {

    String maxNumberOfLostItems = readFile(PATRON_BLOCK_CONDITIONS
      +"/max_number_of_lost_items_no_flat_set_to_true.json");
    PatronBlockCondition response = putWithStatus(PATRON_BLOCK_CONDITIONS_URL
        + MAX_NUMBER_OF_LOST_ITEMS_CONDITION_ID, maxNumberOfLostItems, SC_UNPROCESSABLE_ENTITY, USER_ID)
      .as(PatronBlockCondition.class);
    List<Map<String, Object>> errors =  (List<Map<String, Object>>) response.getAdditionalProperties().get("errors");
    String message = (String) errors.get(0).get("message");
    assertThat(message, is("One or more blocked actions must be selected for message to be displayed to be used"));
  }
}
