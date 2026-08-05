UPDATE users
SET jsonb = jsonb_set(
	jsonb,
	'{personal,preferredContactTypeIds}',
	jsonb_build_array(jsonb->'personal'->'preferredContactTypeId'),
	true
)
WHERE jsonb->'personal' ? 'preferredContactTypeId'
	AND jsonb->'personal'->'preferredContactTypeId' IS NOT NULL
	AND jsonb->'personal'->'preferredContactTypeId' <> 'null'::jsonb;
