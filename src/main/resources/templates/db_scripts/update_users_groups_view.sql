CREATE OR REPLACE VIEW ${myuniversity}_${mymodule}.users_groups_view AS
SELECT
    u.id,
    u.jsonb,
    g.jsonb AS group_jsonb
FROM
    ${myuniversity}_${mymodule}.users u
LEFT JOIN
    ${myuniversity}_${mymodule}.groups g
ON
    ${myuniversity}_${mymodule}.f_unaccent(u.jsonb ->> 'patronGroup') = (g.jsonb ->> 'id');
