ALTER VIEW users_groups_view AS
SELECT users.id,
       users.jsonb,
       groups.jsonb AS group_jsonb
  FROM users
       LEFT JOIN groups
       ON users.f_unaccent(users.jsonb ->> 'patronGroup'::text) = (groups.jsonb ->> 'id'::text);
