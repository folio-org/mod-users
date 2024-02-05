CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;
DO $$
DECLARE
    new_uuid UUID := public.uuid_generate_v4();
    encryption_key text := 'ThisIsASimpleDefaultKeyToTestIts';
BEGIN
    -- Create the configuration table if not exists
    CREATE TABLE IF NOT EXISTS configuration (
        id uuid PRIMARY KEY DEFAULT public.uuid_generate_v4(),
        configName text UNIQUE,
        jsonb jsonb NOT NULL,
        creation_date timestamp without time zone NOT NULL
    );

    -- Insert data into the configuration table
    INSERT INTO configuration (id, configName, jsonb, creation_date)
    VALUES (
        new_uuid,
        'PROFILE_PICTURE_CONFIG',
        json_build_object(
            'id', new_uuid,
            'enabled', false,
            'configName', 'PROFILE_PICTURE_CONFIG',
            'enabledObjectStorage', false,
            'encryptionKey', encryption_key
        ),
        CURRENT_TIMESTAMP
    )
    ON CONFLICT (configName) DO NOTHING;
END $$;
