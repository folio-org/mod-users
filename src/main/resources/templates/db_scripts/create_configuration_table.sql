CREATE TABLE IF NOT EXISTS configuration (
    configName text PRIMARY KEY,
    jsonb jsonb NOT NULL,
    creation_date timestamp without time zone NOT NULL
);

INSERT INTO ${myuniversity}_${mymodule}.configuration (configName, jsonb, creation_date)
VALUES (
    'PROFILE_PICTURE_CONFIG',
    json_build_object(
      'id','3e1aaa06-0600-4cc9-a112-7a3fb8426eda',
      'enabled', false,
      'enabledObjectStorage', false
      ),
      CURRENT_TIMESTAMP
    )
    ON CONFLICT DO NOTHING;
