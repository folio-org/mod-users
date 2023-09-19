UPDATE users
SET jsonb = jsonb_set(jsonb, '{type}', '"system"')
WHERE jsonb->'personal'->>'lastName' ILIKE 'System%'
AND NOT jsonb ? 'type';
