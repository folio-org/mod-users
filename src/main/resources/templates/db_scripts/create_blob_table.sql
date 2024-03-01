CREATE TABLE IF NOT EXISTS profile_picture (
    id uuid PRIMARY KEY,
    profile_picture_blob BYTEA,
    HMAC BYTEA,
    picture_details jsonb NOT NULL
);
