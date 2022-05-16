package org.folio.moduserstest;

import static io.restassured.RestAssured.given;
import static io.restassured.http.ContentType.JSON;
import static io.vertx.core.http.HttpMethod.DELETE;
import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;
import static io.vertx.core.http.HttpMethod.PUT;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_NOT_FOUND;
import static java.net.HttpURLConnection.HTTP_NO_CONTENT;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.folio.HttpStatus.HTTP_UNPROCESSABLE_ENTITY;
import static org.folio.moduserstest.RestITSupport.HTTP_LOCALHOST;
import static org.folio.moduserstest.RestITSupport.delete;
import static org.folio.moduserstest.RestITSupport.get;
import static org.folio.moduserstest.RestITSupport.post;
import static org.folio.moduserstest.RestITSupport.put;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.utils.TenantInit;
import org.folio.support.Group;
import org.folio.support.Groups;
import org.folio.support.Personal;
import org.folio.support.User;
import org.folio.support.Users;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.extension.ExtendWith;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import lombok.SneakyThrows;

@ExtendWith(VertxExtension.class)
@Timeout(value = 20, unit = SECONDS)
class GroupIT {
  private static final Logger log = LogManager.getLogger(GroupIT.class);

  private static int port;
  private final String userUrl = HTTP_LOCALHOST + RestITSupport.port() + "/users";
  private final String groupUrl = HTTP_LOCALHOST + RestITSupport.port() + "/groups";

  @BeforeAll
  public static void beforeAll(Vertx vertx, VertxTestContext context) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());

    port = NetworkUtils.nextFreePort();
    RestITSupport.setUp(port);

    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.port = port;

    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku", "diku", WebClient.create(vertx));
    DeploymentOptions options = new DeploymentOptions()
      .setConfig(new JsonObject().put("http.port", port));

    vertx.deployVerticle(RestVerticle.class.getName(), options, context.succeeding(res -> {
      TenantAttributes ta = new TenantAttributes();
      ta.setModuleTo("mod-users-1.0.0");
      List<Parameter> parameters = new LinkedList<>();
      parameters.add(new Parameter().withKey("loadReference").withValue("false"));
      parameters.add(new Parameter().withKey("loadSample").withValue("false"));
      ta.setParameters(parameters);

      TenantInit.init(tenantClient, ta).onComplete(context.succeedingThenComplete());
    }));
  }

  @BeforeEach
  public void beforeEach() {
    deleteAllUsers();
    deleteAllGroups();
  }

  @Test
  void canCreateANewGroup() {
    var group = Group.builder()
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = createGroup(group);

    assertThat(createdGroup.getId(), is(notNullValue()));
    assertThat(createdGroup.getGroup(), is("New Group"));
    assertThat(createdGroup.getDesc(), is("Group description"));
    assertThat(createdGroup.getExpirationOffsetInDays(), is(365));
  }

  @Test
  void canUpdateAGroup() {
    var group = Group.builder()
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = createGroup(group);

    updateGroup(Group.builder()
      .id(createdGroup.getId())
      .group("A new name")
      .desc("A new description")
      .expirationOffsetInDays(365)
      .build());

    final var updatedGroup = getGroup(createdGroup.getId());

    assertThat(updatedGroup.getGroup(), is("A new name"));
    assertThat(updatedGroup.getDesc(), is("A new description"));
  }

  @Test
  void canDeleteAGroup() {
    var group = Group.builder()
      .group("New Group")
      .desc("Group description")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = createGroup(group);

    deleteGroup(createdGroup.getId());
    getGroup(createdGroup.getId(), HTTP_NOT_FOUND);
  }

  @Test
  void canGetAllGroups() {
    createGroup(Group.builder()
      .group("First new group")
      .desc("First group description")
      .expirationOffsetInDays(365)
      .build());

    createGroup(Group.builder()
      .group("Second new group")
      .desc("Second group description")
      .expirationOffsetInDays(365)
      .build());

    final var groups = getAllGroups();

    assertThat(groups.getTotalRecords(), is(2));

    final var firstGroup = groups.getGroupByName("First new group");

    assertThat("[First new group] exists in collection", firstGroup, is(notNullValue()));
    assertThat(firstGroup.getDesc(), is("First group description"));

    final var secondGroup = groups.getGroupByName("Second new group");

    assertThat("[Second new group] exists in collection", secondGroup, is(notNullValue()));
    assertThat(secondGroup.getDesc(), is("Second group description"));
  }

  //These tests should  be in the integration tests for users not groups
  //they can be moved when the users integration tests are improved
  @Test
  void canFindActiveUsers() {
    createUser(User.builder()
      .username("steve")
      .active(true)
      .build());

    createUser(User.builder()
      .username("joanne")
      .active(false)
      .build());

    createUser(User.builder()
      .username("jenna")
      .active(true)
      .build());

    final var activeUsers = getUsers("active=true");

    assertThat(activeUsers.getTotalRecords(), is(2));
  }

  @Test
  @SneakyThrows
  void test2Group() {
    var fooGroup = Group.builder()
      .group("librarianFOO")
      .desc("yet another basic lib group")
      .expirationOffsetInDays(365)
      .build();

    final var createdGroup = createGroup(fooGroup);

    final var firstUser = createUser(User.builder()
      .username("jhandley")
      .active(true)
      .patronGroup(createdGroup.getId()).build());

    final var userID = firstUser.getId();

    /*
      add the same user name again
     */
    CompletableFuture<Response> addUserCF2 = send(
      userUrl, POST, createUser(null, "jhandley", createdGroup.getId()).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse2 = addUserCF2.get(5, SECONDS);
    assertThat(addUserResponse2.code, is(422));
    log.info(addUserResponse2.body
      + "\nStatus - " + addUserResponse2.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    /*
      add the same user again with same id
     */
    CompletableFuture<Response> addUserCF3 = send(
      userUrl, POST, createUser(userID, "jhandley", createdGroup.getId()).encode(),
      HTTPResponseHandlers.json());
    Response addUserResponse3 = addUserCF3.get(5, SECONDS);
    assertThat(addUserResponse3.code, is(422));
    log.info(addUserResponse3.body
      + "\nStatus - " + addUserResponse3.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    /*
      add a user again with non existent patron group
     */
    CompletableFuture<Response> addUserCF4 = send(userUrl, POST,
      createUser(null, "jhandley2nd", "10c19698-313b-46fc-8d4b-2d00c6958f5d").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4 = addUserCF4.get(5, SECONDS);
    assertThat(addUserResponse4.code, is(HTTP_BAD_REQUEST));
    log.info(addUserResponse4.body
      + "\nStatus - " + addUserResponse4.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    /*
      add a user again with invalid uuid
     */
    CompletableFuture<Response> addUserCF4a = send(
      userUrl, POST, createUser(null, "jhandley2nd", "invalid-uuid").encode(),
      HTTPResponseHandlers.empty());
    Response addUserResponse4a = addUserCF4a.get(5, SECONDS);
    assertThat(addUserResponse4a.code, is(HTTP_UNPROCESSABLE_ENTITY.toInt()));
    log.info(addUserResponse4a.body
      + "\nStatus - " + addUserResponse4a.code + " at " + System.currentTimeMillis() + " for " + userUrl);

    /*
      update a user again with non existent patron group
     */
    CompletableFuture<Response> updateUserCF = send(userUrl + "/" + userID, PUT, createUser(userID, "jhandley2nd",
      "20c19698-313b-46fc-8d4b-2d00c6958f5d").encode(), HTTPResponseHandlers.empty());
    Response updateUserResponse = updateUserCF.get(5, SECONDS);
    assertThat(updateUserResponse.code, is(HTTP_BAD_REQUEST));
    log.info(updateUserResponse.body
      + "\nStatus - " + updateUserResponse.code + " at " + System.currentTimeMillis() + " for " + userUrl + "/" + userID);

    /*
      update a user again with existent patron group
     */
    CompletableFuture<Response> updateUser2CF = send(userUrl + "/" + userID, PUT,
      createUser(userID, "jhandley2nd", createdGroup.getId()).encode(),
      HTTPResponseHandlers.empty());
    Response updateUser2Response = updateUser2CF.get(5, SECONDS);
    assertThat(updateUser2Response.code, is(HTTP_NO_CONTENT));
    log.info(updateUser2Response.body
      + "\nStatus - " + updateUser2Response.code + " at " + System.currentTimeMillis() + " for " + userUrl + "/" + userID);

    /*
      try to get via cql
     */
    String cqlURL = groupUrl + "?query=group==librarianFOO";
    CompletableFuture<Response> cqlCF = send(cqlURL, GET, null, HTTPResponseHandlers.json());
    Response cqlResponse = cqlCF.get(5, SECONDS);
    assertThat(cqlResponse.code, is(HTTP_OK));
    log.info(cqlResponse.body
      + "\nStatus - " + cqlResponse.code + " at " + System.currentTimeMillis() + " for " + cqlURL);
    assertThat(cqlResponse.body.getInteger("totalRecords"), is(1));

    /*
      delete a group - should fail as there is a user associated with the group
     */
    String delete1URL = groupUrl + "/" + createdGroup.getId();
    CompletableFuture<Response> delete1CF = send(delete1URL, DELETE, null, HTTPResponseHandlers.empty());
    Response delete1Response = delete1CF.get(5, SECONDS);
    assertThat(delete1Response.code, is(HTTP_BAD_REQUEST));
    log.info(delete1Response.body
      + "\nStatus - " + delete1Response.code + " at " + System.currentTimeMillis() + " for " + delete1URL);

    /*
      delete a nonexistent group - should return 404
     */
    String deleteNEGURL = groupUrl + "/a492ffd2-b848-48bf-b716-1a645822279e";
    CompletableFuture<Response> deleteNEGCF = send(deleteNEGURL, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteNEGResponse = deleteNEGCF.get(5, SECONDS);
    assertThat(deleteNEGResponse.code, is(HTTP_NOT_FOUND));
    log.info(deleteNEGResponse.body
      + "\nStatus - " + deleteNEGResponse.code + " at " + System.currentTimeMillis() + " for " + deleteNEGURL);

    /*
      try to add a duplicate group
     */
    CompletableFuture<Response> dupCF = send(groupUrl, POST,
      "{\"group\": \"librarianFOO\",\"desc\": \"yet another basic lib group\", \"expirationOffsetInDays\": 365}", HTTPResponseHandlers.json());
    Response dupResponse = dupCF.get(5, SECONDS);
    assertThat(dupResponse.code, is(422));
    log.info(dupResponse.body
      + "\nStatus - " + dupResponse.code + " at " + System.currentTimeMillis() + " for " + groupUrl);

    /*
      get a group bad id
     */
    String getBadIDURL = groupUrl + "/3748ec8d-8dbc-4717-819d-87c839e6905e";
    CompletableFuture<Response> getBadIDCF = send(getBadIDURL, GET, null, HTTPResponseHandlers.empty());
    Response getBadIDResponse = getBadIDCF.get(5, SECONDS);
    assertThat(getBadIDResponse.code, is(HTTP_NOT_FOUND));
    log.info(getBadIDResponse.body
      + "\nStatus - " + getBadIDResponse.code + " at " + System.currentTimeMillis() + " for " + getBadIDURL);

    /*
      delete a group with users should fail
     */
    String delete = groupUrl + "/" + createdGroup.getId();
    CompletableFuture<Response> deleteCF = send(delete, DELETE, null, HTTPResponseHandlers.empty());
    Response deleteResponse = deleteCF.get(5, SECONDS);
    assertThat(deleteResponse.code, is(HTTP_BAD_REQUEST));
    log.info(deleteResponse.body
      + "\nStatus - " + deleteResponse.code + " at " + System.currentTimeMillis() + " for " + delete);

    var barGroup = createGroup(Group.builder()
      .group("librarianBAR")
      .desc("and yet another basic lib group")
      .build());

     createUser(User.builder()
      .username("jhandley0")
      .active(true)
      .personal(Personal.builder().lastName("Triangle").build())
      .patronGroup(barGroup.getId()).build());

    createUser(User.builder()
      .username("jhandley1")
      .active(true)
      .personal(Personal.builder().lastName("Triangle").build())
      .patronGroup(barGroup.getId()).build());

    final var usersSortedByGroupDesc = getUsers("cql.allRecords=1 sortBy patronGroup.group/sort.descending");

    assertThat(usersSortedByGroupDesc.getTotalRecords(), is(3));
    assertThat(usersSortedByGroupDesc.getUsers().get(0).getUsername(), is("jhandley2nd"));

    final var usersSortedByGroupAsc = getUsers("cql.allRecords=1 sortBy patronGroup.group/sort.ascending");

    assertThat(usersSortedByGroupAsc.getTotalRecords(), is(3));
    assertThat(usersSortedByGroupAsc.getUsers().get(0).getUsername(), is("jhandley0"));

    final var usersFilteredByGroupName = getUsers("patronGroup.group=librarianFOO sortBy patronGroup.group/sort.descending");

    assertThat(usersFilteredByGroupName.getTotalRecords(), is(1));

    final var usersFilteredByGroupNameThatDoesNotExist = getUsers("patronGroup.group=abc*");

    assertThat(usersFilteredByGroupNameThatDoesNotExist.getTotalRecords(), is(0));
  }

  private CompletableFuture<Response> send(String url, HttpMethod method, String content,
      Function<HttpResponse<Buffer>, Response> handler) {

    Future<HttpResponse<Buffer>> httpResponse;

    switch (method.name()) {
      case "GET":
        httpResponse = get(url);
        break;
      case "POST":
        httpResponse = post(url, defaultString(content));
        break;
      case "PUT":
        httpResponse = put(url, defaultString(content));
        break;
      case "DELETE":
        httpResponse = delete(url);
        break;
      default:
        throw new IllegalArgumentException("Illegal method: " + method);
    }

    CompletableFuture<Response> result = new CompletableFuture<>();

    httpResponse.map(handler)
      .onSuccess(result::complete)
      .onFailure(result::completeExceptionally);

    return result;
  }

  private static JsonObject createUser(String id, String name, String pgId) {
    JsonObject user = new JsonObject();
    if (id != null) {
      user.put("id", id);
    }
    user.put("username", name);
    user.put("patronGroup", pgId);
    user.put("active", true);
    user.put("personal", new JsonObject()
      .put("lastName", "Triangle")
      .put("firstName", "Jack")
    );
    return user;
  }

  @SneakyThrows
  private Group createGroup(Group group) {
    return given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .contentType(JSON)
      .accept("application/json, text/plain")
      .when()
      .body(new ObjectMapper().writeValueAsString(group))
      .post("/groups")
      .then()
      .statusCode(HTTP_CREATED)
      .extract().as(Group.class);
  }

  private Group getGroup(String id) {
    return getGroup(id, HTTP_OK)
      .extract().as(Group.class);
  }

  private ValidatableResponse getGroup(String id, int expectedStatusCode) {
    return given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .accept("application/json, text/plain")
      .when()
      .get("/groups/{id}", Map.of("id", id))
      .then()
      .statusCode(expectedStatusCode);
  }

  private Groups getAllGroups() {
    return given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .accept("application/json, text/plain")
      .when()
      .get("/groups")
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Groups.class);
  }

  @SneakyThrows
  private void updateGroup(Group group) {
    given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .contentType(JSON)
      .accept("application/json, text/plain")
      .when()
      .body(new ObjectMapper().writeValueAsString(group))
      .put("/groups/{id}", Map.of("id", group.getId()))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }

  void deleteAllGroups() {
    final var groups = getAllGroups();

    groups.getUsergroups().forEach(group -> deleteGroup(group.getId()));
  }

  private void deleteGroup(String id) {
    given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .accept("application/json, text/plain")
      .when()
      .delete("/groups/{id}", Map.of("id", id))
      .then()
      .statusCode(HTTP_NO_CONTENT);
  }

  @SneakyThrows
  private User createUser(User userToCreate) {
    return given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .contentType(JSON)
      .accept("application/json, text/plain")
      .when()
      .body(new ObjectMapper().writeValueAsString(userToCreate))
      .post("/users")
      .then()
      .statusCode(HTTP_CREATED)
      .extract().as(User.class);
  }

  private Users getUsers(String query) {
    return given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .accept("application/json, text/plain")
      .when()
      .get("/users?query=" + query)
      .then()
      .statusCode(HTTP_OK)
      .extract().as(Users.class);
  }

  void deleteAllUsers() {
    given()
      .header("X-Okapi-Tenant", "diku")
      .header("X-Okapi-Token", "")
      .header("X-Okapi-Url", "http://localhost:" + port)
      .accept("application/json, text/plain")
      .when()
      .param("query", "cql.allRecords=1")
      .delete("/users")
      .then()
      .statusCode(204);
  }

  private static class Response {
    int code;
    JsonObject body;
  }

  private static class HTTPResponseHandlers {
    static Function<HttpResponse<Buffer>, Response> empty() {
      return response -> {
        Response result = new Response();
        result.code = response.statusCode();
        return result;
      };
    }

    static Function<HttpResponse<Buffer>, Response> json() {
      return response -> {
        Response result = new Response();
        result.code = response.statusCode();
        result.body = response.bodyAsJsonObject();
        return result;
      };
    }
  }
}
