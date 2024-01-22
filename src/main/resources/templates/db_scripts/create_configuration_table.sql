CREATE TABLE IF NOT EXISTS configuration (
    id uuid PRIMARY KEY,
    configName text NOT NULL UNIQUE,
    jsonb jsonb NOT NULL,
    creation_date timestamp without time zone NOT NULL
);

INSERT INTO ${myuniversity}_${mymodule}.configuration (id, configName, jsonb, creation_date)
VALUES (
    '3e1aaa06-0600-4cc9-a112-7a3fb8426eda',
    'PROFILE_PICTURE_CONFIG',
    json_build_object(
      'id','3e1aaa06-0600-4cc9-a112-7a3fb8426eda',
      'enabled', false,
      'enabledObjectStorage', false
      ),
      CURRENT_TIMESTAMP
    )
    ON CONFLICT DO NOTHING;
