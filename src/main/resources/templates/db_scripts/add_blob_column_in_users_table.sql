ALTER TABLE users
  ADD COLUMN IF NOT EXISTS profile_picture_blob BYTEA;
