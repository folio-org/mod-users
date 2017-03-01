CREATE ROLE myuniversity_mymodule PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity_mymodule AUTHORIZATION myuniversity_mymodule;

CREATE EXTENSION IF NOT EXISTS "pgcrypto";

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.users (_id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);

CREATE TABLE IF NOT EXISTS myuniversity_mod_users.groups (
   _id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   jsonb jsonb NOT NULL,
   creation_date date not null default current_timestamp,
   update_date date not null default current_timestamp
   );
-- index to support @> ops, faster than jsonb_ops
CREATE INDEX idxgin_groups ON myuniversity_mod_users.groups USING gin (jsonb jsonb_path_ops);
-- update the update_date column when record is updated
CREATE OR REPLACE FUNCTION update_modified_column_groups()
RETURNS TRIGGER AS $$
BEGIN
-- NEW to indicate updating the new row value
    NEW.update_date = current_timestamp;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_date_groups BEFORE UPDATE ON myuniversity_mod_users.groups FOR EACH ROW EXECUTE PROCEDURE  update_modified_column_groups();

-- join table
CREATE TABLE IF NOT EXISTS myuniversity_mod_users.groups_users (jsonb JSONB NOT NULL);
-- join table composite index to ensure a group/user pair can not be inserted twice
CREATE UNIQUE INDEX group_user_unique_idx ON myuniversity_mod_users.groups_users(((jsonb->>'groupId')::text), ((jsonb->>'userId')::text));
-- join table index to allow fast retrieval of users for a specific group
CREATE INDEX IF NOT EXISTS group_user_ongroup_idx ON myuniversity_mod_users.groups_users(((jsonb->>'group_id')::text));
-- join table index to allow fast retrieval od groups for a specific user
CREATE INDEX IF NOT EXISTS group_user_onuser_idx ON myuniversity_mod_users.groups_users(((jsonb->>'user_id')::text));

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA myuniversity_mymodule TO myuniversity_mymodule;
