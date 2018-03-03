## Not yet released
 * MODUSERS-58: Update shared raml-util. The metadata schema was missing when processing RAML files in a different order.

## 14.5.0 2018-03-01
 * Remove required 'desc' field for usergroups
 * Update to RMB 18.0.1

## 14.4.1 2018-01-16
 * Implement fix to CQL generation to handle users without patrongroups

## 14.4.0 2018-01-12
 * Update to RMB 16.0.3
 * Add standard metadata field support to userdata
 * Fix bug allowing for type-less address types
 * Add check for deleting nonexistent groups

## 14.2.0 2017-08-23
 * Add 'proxyFor' field to userdata

## 14.1.2 2017-08-23
 * Fix bug with numeric IDs

## 14.1.1 2017-07-27
 * Fix MD to contain implementation number in id

## 14.1.0 2017-07-27
 * Update RMB to allow for cross table joins (e.g. sorting by patrongroup names)
 * Adjust CQL handling to accommodate cross table queries
 * Fix bug causing numeric usernames to be treated as integers

## 14.0.0 2017-07-06
 * Enable /addresstypes endpoint for CRUD on address types
 * Add restrictions to user data to require that only one address of a given addresstype exist, and that all addresstypeids have a corresponding address type record
 * Put implementation version into module descriptor

## 13.0.1 2017-06-27
 * Update ModuleDescriptor

## 13.0.0 2017-06-26
 * dateCreated and dateUpdated fields are set automatically on POST/PUT
 * Return sets use 'totalResults' field instead of 'total_results'

## 12.0.0 2017-06-14
 * Change timestamp fields to dateCreated and dateUpdated

## 11.0.0 2017-06-07
 * Replace user JSON property 'preferredContact' with 'preferredContactTypeId'
 * Add array of addresses in user JSON property 'personal'
 * Change dependency to RMB v12.1.3

## 10.1.1 2017-05-26
 * Correct interface version in Module Descriptor

## 10.1.0 2017-05-24
 * Add optional field to users: externalSystemId

## 10.0.0 2017-05-23
 * Change dependency to RMB v12.1.0
 * Disallow additional properties in usergroups model.

## 9.0.0 2017-05-11
 * Change dependency to RMB v11.0.0. Schema now disallows additional properties.
