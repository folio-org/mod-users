CREATE ROLE myuniversity_mymodule PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

GRANT myuniversity_mymodule TO CURRENT_USER;

CREATE SCHEMA myuniversity_mymodule AUTHORIZATION myuniversity_mymodule;

CREATE EXTENSION IF NOT EXISTS "pgcrypto";

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.users (id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.groups (
   id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
   jsonb jsonb NOT NULL,
   creation_date timestamp  not null default current_timestamp,
   update_date timestamp  not null default current_timestamp
   );

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.addresstype (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    jsonb jsonb NOT NULL
    );

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.proxyfor (
	id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
	jsonb jsonb NOT NULL
	);

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.userpin (
	userid UUID PRIMARY KEY DEFAULT gen_random_uuid() REFERENCES users ON DELETE CASCADE,
	jsonb jsonb NOT NULL
	);

-- left join so that we have all the users even if there is no group associated with them
CREATE VIEW myuniversity_mymodule.users_groups_view AS select u.id,u.jsonb as jsonb, g.jsonb as group_jsonb from myuniversity_mymodule.users u
left join myuniversity_mymodule.groups g on u.jsonb->>'patronGroup' = g.jsonb->>'id'; -- order by g.jsonb->>'group' desc;

-- index to support @> ops, faster than jsonb_ops
CREATE INDEX idxgin_groups ON myuniversity_mymodule.groups USING gin (jsonb jsonb_path_ops);
-- create unique index on groupname group
CREATE UNIQUE INDEX group_unique_idx ON myuniversity_mymodule.groups((jsonb->>'group'));
-- update the update_date column when record is updated

-- index to support @> ops, faster than jsonb_ops
CREATE INDEX idxgin_addresstype ON myuniversity_mymodule.addresstype USING gin (jsonb jsonb_path_ops);

CREATE INDEX idxgin_proxyfor ON myuniversity_mymodule.proxyfor USING gin (jsonb jsonb_path_ops);
CREATE UNIQUE INDEX proxy_unique_idx ON myuniversity_mymodule.proxyfor((jsonb->>'userId'), (jsonb->>'proxyUserId'));

CREATE OR REPLACE FUNCTION update_modified_column_groups()
RETURNS TRIGGER AS $$
BEGIN
-- NEW to indicate updating the new row value
    NEW.update_date = current_timestamp;
    RETURN NEW;
END;
$$ language 'plpgsql';
CREATE TRIGGER update_date_groups BEFORE UPDATE ON myuniversity_mymodule.groups FOR EACH ROW EXECUTE PROCEDURE  update_modified_column_groups();

CREATE OR REPLACE FUNCTION set_id_injson_groups3()
RETURNS TRIGGER AS $$
DECLARE
  injectedId text;
BEGIN
-- NEW to indicate updating the new row value
  injectedId = '"'||NEW.id||'"';
  NEW.jsonb = jsonb_set(NEW.jsonb, '{id}' ,  injectedId::jsonb , true);
    RETURN NEW;
END;
$$ language 'plpgsql';
CREATE TRIGGER set_id_injson_groups3 BEFORE INSERT OR UPDATE ON myuniversity_mymodule.groups FOR EACH ROW EXECUTE PROCEDURE  set_id_injson_groups3();

-- join table composite index to ensure a group/user pair can not be inserted twice
-- CREATE UNIQUE INDEX group_user_unique_idx ON myuniversity_mymodule.groups_users(((jsonb->>'groupId')::text), ((jsonb->>'userId')::text));

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA myuniversity_mymodule TO myuniversity_mymodule;
