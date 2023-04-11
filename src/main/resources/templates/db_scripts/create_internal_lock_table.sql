CREATE TABLE IF NOT EXISTS internal_lock (
  lock_name text NOT NULL PRIMARY KEY
);

INSERT INTO internal_lock(lock_name) VALUES ('user_outbox') ON CONFLICT DO NOTHING;
