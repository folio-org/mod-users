ALTER TABLE user_tenant
  ADD COLUMN IF NOT EXISTS centralTenantId text,
  ADD COLUMN IF NOT EXISTS email text,
  ADD COLUMN IF NOT EXISTS mobilePhoneNumber text,
  ADD COLUMN IF NOT EXISTS phoneNumber text;
