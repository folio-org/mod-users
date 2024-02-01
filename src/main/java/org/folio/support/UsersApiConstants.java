package org.folio.support;

import static org.apache.commons.io.FileUtils.ONE_MB;

public class UsersApiConstants {

  private UsersApiConstants() {}
  public static final long MAX_DOCUMENT_SIZE = 10 * ONE_MB;
  public static final String TABLE_NAME_PROFILE_PICTURE = "profile_picture";
  public static final String TABLE_NAME_USERS = "users";
  public static final String TABLE_NAME_CONFIG = "configuration";
  public static final String PROFILE_PICTURE_FOLDER = "Profile-Pictures";
  public static final String SAVE_PROFILE_PICTURE_SQL = "INSERT INTO %s.%s (id, profile_picture_blob) VALUES ($1, $2)";
  public static final String GET_CONFIGURATION_SQL = "SELECT * FROM %s.%s WHERE configName = 'PROFILE_PICTURE_CONFIG'";
  public static final String DELETE_USERS_SQL = "DELETE from %s.%s";
  public static final String UPDATE_PROFILE_PICTURE_SQL = "UPDATE %s.%s set profile_picture_blob = $1 where id = $2 returning id, profile_picture_blob";
  public static final String DELETE_PROFILE_PICTURE_SQL = "DELETE from %s.%s where id = $1";
  public static final String GET_PROFILE_PICTURE_SQL = "SELECT * from %s.%s WHERE id = $1";
  public static final String GET_CONFIG_SQL = "SELECT * from %s.%s";
  public static final String RETURNING_USERS_ID_SQL = "RETURNING id";
  public static final String ID = "id";
  public static final String BLOB = "profile_picture_blob";
  public static final String VIEW_NAME_USER_GROUPS_JOIN = "users_groups_view";
  public static final String USERNAME_ALREADY_EXISTS = "users_username_idx_unique";
  public static final String BARCODE_ALREADY_EXISTS = "users_barcode_idx_unique";
  public static final String INVALID_USERNAME_ERROR = "The user with the ID %s must have a username since consortium mode is enabled";
  public static final String INVALID_USER_TYPE_ERROR = "An invalid user type has been populated to a user, allowed values: %s";
  public static final String DUPLICATE_BARCODE_ERROR = "This barcode has already been taken";
  public static final String DUPLICATE_USERNAME_ERROR = "User with this username already exists";
  public static final String DUPLICATE_ID_ERROR = "User with this id already exists";
  public static final String CONFIG_NAME = "configname";
  public static final String JSONB = "jsonb";
  public static final String ENABLED = "enabled";
  public static final String ENABLED_OBJECT_STORAGE = "enabledObjectStorage";
}
