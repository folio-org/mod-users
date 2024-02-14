DROP TABLE IF EXISTS profile_picture;

CREATE TABLE profile_picture (
    id uuid PRIMARY KEY,
    profile_picture_blob BYTEA,
    HMAC BYTEA
);
