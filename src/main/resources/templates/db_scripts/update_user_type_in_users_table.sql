UPDATE users
SET jsonb = jsonb_set(jsonb, '{type}', '"system"')
WHERE lower(f_unaccent(jsonb->>'username')) IN ('mod-search', 'pub-sub', 'mod-innreach', 'data-export-system-user', 'system-user')
AND NOT jsonb ? 'type';
