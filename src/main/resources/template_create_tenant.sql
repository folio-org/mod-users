CREATE ROLE myuniversity PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity AUTHORIZATION myuniversity;

CREATE TABLE myuniversity.users (_id SERIAL PRIMARY KEY, jsonb JSONB NOT NULL);

GRANT ALL ON myuniversity.users TO myuniversity;
GRANT ALL ON myuniversity.users__id_seq TO myuniversity;
