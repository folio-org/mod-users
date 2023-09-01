CREATE TABLE IF NOT EXISTS outbox_event_log (
  event_id uuid NOT NULL PRIMARY KEY,
  entity_type text NOT NULL,
  action text NOT NULL,
  action_date timestamptz NOT NULL,
  payload jsonb
);

CREATE INDEX IF NOT EXISTS outbox_event_log_action_date_idx ON outbox_event_log USING BTREE (action_date);

ALTER TABLE outbox_event_log ADD COLUMN IF NOT EXISTS is_personal_data_changed boolean;
