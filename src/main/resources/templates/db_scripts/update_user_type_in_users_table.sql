UPDATE users
SET jsonb = jsonb_set(jsonb, '{type}', '"System"')
WHERE jsonb->'personal'->>'lastName' ILIKE 'System%'
AND NOT jsonb ? 'type';
