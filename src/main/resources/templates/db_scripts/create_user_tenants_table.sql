CREATE TABLE IF NOT EXISTS user_tenant (
  id uuid PRIMARY KEY,
  user_id uuid NOT NULL,
  username text NOT NULL,
  tenant_id text NOT NULL,
  creation_date timestamp without time zone
);

DROP INDEX IF EXISTS ${myuniversity}_${mymodule}.username_idx;
CREATE INDEX IF NOT EXISTS username_idx ON user_tenant USING BTREE (LOWER(f_unaccent(username));
