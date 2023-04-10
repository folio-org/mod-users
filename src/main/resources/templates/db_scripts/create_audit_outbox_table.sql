CREATE TABLE IF NOT EXISTS outbox_event_log (
  event_id uuid NOT NULL PRIMARY KEY,
  entity_type text NOT NULL,
  action text NOT NULL,
  action_date timestamptz NOT NULL,
  payload jsonb
);
