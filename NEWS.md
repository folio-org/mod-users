## 19.0.0 IN-PROGRESS

* Made AddressTypeId field required for all user addresses (MODUSERS-212)

## 18.3.0 2022-06-13

* Added index to search by perferredFirstName (MODUSERS-310)
* Fixed issue posting user with text custom field with null values (MODUSERS-306)

## 18.2.0 2022-02-22

* Upgrade to RMB 33.2.4 and Folio Service Tools 1.7.2. (CVE-2021-44228) (MODUSERS-295, MODUSERS-301)

## 18.1.1 2021-10-04

* Update RMB to 33.1.1 and Vert.x to 4.1.4 (MODUSERS-272)
* Move helper methods to bottom of UsersAPIIT (MODUSERS-267)
* Build fails with LANG=de\_DE.UTF-8 (MODUSERS-259)

## 18.1.0 2021-09-28

* Characters in department name and code now properly saved (MODUSERS-260)
* Creation of new user with invalid addresstype id now results in 422 error (MODUSERS-262)
* Added delete by cql endpoint for users (MODUSERS-122)

## 18.0.0 2021-06-16

* `embed_postgres` command line option is no longer supported (MODUSERS-255)
* Upgrades to RAML Module Builder 33.0.0 (MODUSERS-255)
* Upgrades to 4.1.0.CR1 (MODUSERS-255)

## 17.3.0 2021-03-09

* Groups can define a default period after which a user should expire (MODUSERS-234)
* Uses a `_timer` interface to periodically expire users every minute (MODUSERS-244)
* Updates Vert.x to version 4 (MODUSERS-242)
* Provides `users 15.3`
* Provides `_tenant 2.0`

## 17.2.0 2020-10-06

* Allows users to be assigned to departments (MODUSERS-158)
* Provides `users 15.2` (MODUSERS-158)
* Requires JDK 11 (MODUSERS-215)
* Upgraded to RAML Module Builder 31.1.0 (MODUSERS-214, MODUSERS-220)

## 17.1.0 2020-07-07

* updated mod-custom-fields version to 1.4.1
* MODUSERS-196 - BE - Preferred name field on user record
* MODCFIELDS-46 - Custom Field Option: Provide an indication that an option has been saved to a record

## 17.0.0 2020-06-10

* Custom fields can have multiple values (MODUSERS-201)
* Selectable options for custom fields must be uniquely identified (MODUSERS-201)
* Provides `custom-fields 2.0`
* No longer provides `patron-block-conditions`
* No longer provides `patron-block-limits`
* Upgrades to RAML Module Builder 30.0.2 (MODUSERS-195)

## 16.1.0 2020-03-11

* `id` no longer required when creating a new user (MODUSERS-187)
* Removes custom field values when custom field is deleted (MODUSERS-172)
* Stores patron block conditions (MODUSERS-174, MODUSERS-176)
* Stores patron block limits (MODUSERS-175, MODUSERS-177, MODUSERS-180)
* Streams responses to get users (MODUSERS-114, MODUSERS-171, MODUSERS-173)
* Upgrades to `RAML Module Builder 29.3.0` (MODUSERS-164, MODUSERS-168)
* Upgrades to `mod-custom-fields 1.3.0` (MODUSERS-179, MODUSERS-183)
* Provides `custom-fields 1.2`
* Provides `patron-block-conditions 0.1`
* Provides `patron-block-limits 0.1`

## 16.0.0 2019-12-04

 * [MODUSERS-118](https://issues.folio.org/browse/MODUSERS-118) Reject duplicate user barcode
 * [MODUSERS-126](https://issues.folio.org/browse/MODUSERS-126) [MODUSERS-131](https://issues.folio.org/browse/MODUSERS-131) [MODUSERS-142](https://issues.folio.org/browse/MODUSERS-142) Upgrade to RMB 25, RMB 26, RMB 27
 * [MODUSERS-138](https://issues.folio.org/browse/MODUSERS-138) POST requests to /users fail - syntax error at or near ")"
 * [MODUSERS-143](https://issues.folio.org/browse/MODUSERS-143) PUT and POST requests fail with an error related to a null metadata field
 * [MODUSERS-147](https://issues.folio.org/browse/MODUSERS-147) "Username already exists" error when creating second user with null username
 * [MODUSERS-151](https://issues.folio.org/browse/MODUSERS-151) Exclude whitespace in the username when creating a new user
 * [MODUSERS-134](https://issues.folio.org/browse/MODUSERS-134) [MODUSERS-146](https://issues.folio.org/browse/MODUSERS-146) [MODUSERS-149](https://issues.folio.org/browse/MODUSERS-149) Add Custom fields to user schema
 * [MODUSERS-153](https://issues.folio.org/browse/MODUSERS-153) Calculate custom field usage on User records
 * [MODUSERS-129](https://issues.folio.org/browse/MODUSERS-129) Use PgUtil / refactor
 * [MODUSERS-130](https://issues.folio.org/browse/MODUSERS-130) Updating user without metadata erroneously sets createdDate
 * [MODUSERS-135](https://issues.folio.org/browse/MODUSERS-135) Address index warnings from perf test
 * [MODUSERS-150](https://issues.folio.org/browse/MODUSERS-150) Add indexes to improve performance
 * [MODUSERS-160](https://issues.folio.org/browse/MODUSERS-160) [FOLIO-2358](https://issues.folio.org/browse/FOLIO-2358) Manage container memory, switch to alpine-jre-openjdk8 Docker container
 * [FOLIO-2256](https://issues.folio.org/browse/FOLIO-2256) Publish container to ci kubernetes when release/snap build
 * [FOLIO-2234](https://issues.folio.org/browse/FOLIO-2234) Add LaunchDescriptor settings

## 15.6.2 2019-09-11
 * [MODUSERS-138](https://issues.folio.org/browse/MODUSERS-138) POST requests to /users fail
 * [MODUSERS-147](https://issues.folio.org/browse/MODUSERS-147) "Username already exists" error when creating second user with null username

## 15.6.1 2019-07-23
 * MODUSERS-129 Use PgUtil / refactor
 * MODUSERS-131 Upgrade to RMB 26.2.2

## 15.6.0 2019-06-12
 * MODUSERS-125 User (sort by group) search slow in 2.1 Bugfest env
 * MODUSERS-127 Improve user expiration query performance

## 15.5.0 2019-05-10
 * MODUSERS-115 Update to RMB 24 / CQLPG 4.0.0
 * MODUSERS-106 Add index for searching by "username"
 * MODUSERS-109 Avoid users load sample hack

## 15.4.0 2019-03-15
 * MODUSERS-103 use loadSample/loadReference to load sample and reference data
 * MODUSERS-108	PUT new User returns 204
 * MODUSERS-102 pom.xml: Replace tab by spaces, fixing indentation
 * MODUSERS-105	Update to Vert.X 3.5.4 / RMB 23.8.0

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
