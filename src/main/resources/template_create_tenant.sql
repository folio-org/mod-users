CREATE ROLE myuniversity PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity AUTHORIZATION myuniversity;

CREATE TABLE myuniversity.user (_id SERIAL PRIMARY KEY, jsonb JSONB NOT NULL);

GRANT ALL ON myuniversity.user TO myuniversity;
GRANT ALL ON myuniversity.user__id_seq TO myuniversity;
