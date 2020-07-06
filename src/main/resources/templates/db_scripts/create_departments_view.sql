CREATE OR REPLACE VIEW departments_view AS
  SELECT d.id AS id, jsonb_set(d.jsonb, '{attributes,usageNumber}', to_jsonb(COUNT(u.jsonb->>'departments'))) AS jsonb
  FROM departments d
  LEFT JOIN users u ON d.jsonb->>'id' = ANY (SELECT jsonb_array_elements_text(u.jsonb->'departments'))
  GROUP BY d.id;
