ALTER TABLE user_tenant
  ADD COLUMN IF NOT EXISTS central_tenant_id text,
  ADD COLUMN IF NOT EXISTS email text,
  ADD COLUMN IF NOT EXISTS mobile_phone_number text,
  ADD COLUMN IF NOT EXISTS phone_number text,
  ADD COLUMN IF NOT EXISTS barcode text,
  ADD COLUMN IF NOT EXISTS external_system_id text;

DROP INDEX IF EXISTS ${myuniversity}_${mymodule}.username_idx;
CREATE INDEX IF NOT EXISTS username_idx ON user_tenant USING BTREE (LOWER(f_unaccent(username)));
