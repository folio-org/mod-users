CREATE ROLE myuniversity_mymodule PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity_mymodule AUTHORIZATION myuniversity_mymodule;

CREATE EXTENSION IF NOT EXISTS "pgcrypto";

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.users (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.groups (
   id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   jsonb jsonb NOT NULL,
   creation_date date not null default current_timestamp,
   update_date date not null default current_timestamp
   );
-- index to support @> ops, faster than jsonb_ops
CREATE INDEX idxgin_groups ON myuniversity_mymodule.groups USING gin (jsonb jsonb_path_ops);
-- create unique index on groupname group
CREATE UNIQUE INDEX group_unique_idx ON myuniversity_mymodule.groups((jsonb->>'group'));
-- update the update_date column when record is updated
CREATE OR REPLACE FUNCTION update_modified_column_groups()
RETURNS TRIGGER AS $$
BEGIN
-- NEW to indicate updating the new row value
    NEW.update_date = current_timestamp;
    RETURN NEW;
END;
$$ language 'plpgsql';
CREATE TRIGGER update_date_groups BEFORE UPDATE ON myuniversity_mymodule.groups FOR EACH ROW EXECUTE PROCEDURE  update_modified_column_groups();

-- join table composite index to ensure a group/user pair can not be inserted twice
-- CREATE UNIQUE INDEX group_user_unique_idx ON myuniversity_mymodule.groups_users(((jsonb->>'groupId')::text), ((jsonb->>'userId')::text));

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA myuniversity_mymodule TO myuniversity_mymodule;
