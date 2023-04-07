CREATE TABLE IF NOT EXISTS internal_lock (
  lock_name text NOT NULL PRIMARY KEY
);

INSERT INTO internal_lock(lock_name) VALUES ('audit_outbox') ON CONFLICT DO NOTHING;
