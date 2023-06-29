ALTER TABLE user_tenant
  ADD COLUMN IF NOT EXISTS central_tenant_id text,
  ADD COLUMN IF NOT EXISTS email text,
  ADD COLUMN IF NOT EXISTS mobile_phone_number text,
  ADD COLUMN IF NOT EXISTS phone_number text;
