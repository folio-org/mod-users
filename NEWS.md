## 15.3.0 2018-12-05
 * Clean up RAML directory (MODUSERS-84)
 * Add missing documentation for fieldnames in schemas (MODUSERS-87)
 * Correct exception thrown for CQL errors (MODUSERS-88)
 * Correct issue with pagaeable trait in RAML (MODUSERS-89)
 * Put numeric username; add schemas for type detection (MODUSERS-90)
 * Enforce order in which unit tests fire (MODUSERS-92)
 * Correct issue with CQL field name validation (MODUSERS-93)
 * Correct issue with database primary key assignment (MODUSERS-94, MODUSERS-95)
 * Correct issue with type determination for values in CQL queries (MODUSERS-96)
 * Fix issue that causes mod-users to hang when a bad query to /groups is received (MODUSERS-97)
 * Expand unit test coverage (MODUSERS-100)

## 15.2.0 2018-09-12
 * Upgrade to RMB 21.x.x, use RAML 1.0
 * Change user expiration to use the periodic API instead of deserializer.
 * Remove 'meta' field from proxyFor schema
 * Update raml definitions to include metadata schema
 * Add standard metadata field to proxyfor schema
 * Define fields in proxyFor schema to replace the use of catch-all 'meta' field
 * Allow groups and addresstypes to be supplied an optional id on creation
 * Make 'username' field now optional for User records
 * Enable standard metadata on AddressType records
 * Add custom JSON deserializer to automatically set "active" to false if the user expiration date has passed
 * Update RAML traits to raise (to maxint) the number of results that can be requested
 * MODUSERS-58: Update shared raml-util. The metadata schema was missing when processing RAML files in a different order.
 * Remove required 'desc' field for usergroups
 * Implement fix to CQL generation to handle users without patrongroups
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
