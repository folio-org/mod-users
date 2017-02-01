CREATE ROLE myuniversity PASSWORD 'myuniversity' NOSUPERUSER NOCREATEDB INHERIT LOGIN;

CREATE SCHEMA myuniversity_mymodule AUTHORIZATION myuniversity;

CREATE EXTENSION IF NOT EXISTS 'pgcrypto';

CREATE TABLE IF NOT EXISTS myuniversity_mymodule.users (_id UUID PRIMARY KEY DEFAULT gen_random_uuid(), jsonb JSONB NOT NULL);

GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA myuniversity_mymodule TO myuniversity;
