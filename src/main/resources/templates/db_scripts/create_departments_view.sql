CREATE INDEX IF NOT EXISTS users_departments_idx_gin ON ${myuniversity}_${mymodule}.users USING GIN ((jsonb -> 'departments'));

CREATE OR REPLACE VIEW ${myuniversity}_${mymodule}.departments_view AS
  SELECT d.id AS id, jsonb_set(d.jsonb, '{usageNumber}', to_jsonb(COUNT(u))) AS jsonb
  FROM ${myuniversity}_${mymodule}.departments d
  LEFT JOIN ${myuniversity}_${mymodule}.users u ON u.jsonb -> 'departments' ? d.id::text
  GROUP BY d.id;
