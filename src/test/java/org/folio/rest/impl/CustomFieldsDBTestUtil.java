package org.folio.rest.impl;

import static org.folio.test.util.DBTestUtil.deleteFromTable;
import static org.folio.test.util.DBTestUtil.getAll;
import static org.folio.test.util.DBTestUtil.save;
import static org.folio.test.util.TestUtil.STUB_TENANT;

import java.util.List;

import io.vertx.core.Vertx;

import org.folio.rest.jaxrs.model.CustomField;

public class CustomFieldsDBTestUtil {

  public static final String CUSTOM_FIELDS_TABLE = "custom_fields";

  private CustomFieldsDBTestUtil() {
  }

  public static void deleteAllCustomFields(Vertx vertx) {
    deleteFromTable(vertx, CUSTOM_FIELDS_TABLE, STUB_TENANT);
  }

  public static List<CustomField> getAllCustomFields(Vertx vertx) {
    return getAll(CustomField.class, vertx, CUSTOM_FIELDS_TABLE, STUB_TENANT);
  }

  public static void saveCustomField(String id, CustomField customField, Vertx vertx) {
    save(id, customField, vertx, CUSTOM_FIELDS_TABLE, STUB_TENANT);
  }
}
