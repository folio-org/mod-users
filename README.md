# mod-users

Copyright (C) 2016-2025 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

Module to provide central user management for FOLIO systems.

## Prerequisites

* Java 21 JDK
* Maven 3.3.9

## Additional information

The [raml-module-builder](https://github.com/folio-org/raml-module-builder) framework.

Other [modules](https://dev.folio.org/source-code/#server-side).

Other FOLIO Developer documentation is at [dev.folio.org](https://dev.folio.org/)

### Issue tracker

See project [MODUSERS](https://issues.folio.org/browse/MODUSERS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### ModuleDescriptor

See the built `target/ModuleDescriptor.json` for the interfaces that this module
requires and provides, the permissions, and the additional module metadata.

### API documentation

This module's [API documentation](https://dev.folio.org/reference/api/#mod-users).

### Code analysis

[SonarQube analysis](https://sonarcloud.io/dashboard?id=org.folio%3Amod-users).

### Download and configuration

The built artifacts for this module are available.
See [configuration](https://dev.folio.org/download/artifacts) for repository access,
and the [Docker image](https://hub.docker.com/r/folioorg/mod-users/).

# Configuration setting for Profile-Picture Feature
To enable this feature for a tenant, we need to perform one of the two ways described below

## 1. Configuration using `users.settings` interface

This is a preferred method of configuring Profile-Picture feature.

### Permissions

###### users.settings.all - to fully manage user profile-picture (and other) settings

or, depending on the required operations, any of the below permissions can be used

###### users.settings.collection.get - to view all user settings items
###### users.settings.item.get       - to view a specific user settings item
###### users.settings.item.post      - to create a new user settings item
###### users.settings.item.put       - to update an existing user settings item
###### users.settings.item.delete    - to delete a user settings item

### Invoke POST endpoint to create setting
#### Example request to create profile-picture configuration

`POST /user/settings`
```json
{
  "id": "3346f338-593c-447e-924e-bbfb2a883715",
  "scope": "mod-users",
  "key": "PROFILE_PICTURE_CONFIG",
  "value": {
    "enabled": true,
    "maxFileSize": 5.0,
    "encryptionKey": "ThisIsASimpleDefaultKeyToTestIts",
    "enabledObjectStorage": false
  }
}
```

Required parameters:
* `key = "PROFILE_PICTURE_CONFIG"`
* `scope = "mod-users"`
* `value.enabled` - true/false to enable/disable profile-picture feature
* `value.maxFileSize` - maximum file size in megabytes (range 0.1 to 10)
* `value.encryptionKey` - encryption key to encrypt/decrypt profile pictures
* `value.enabledObjectStorage` - true/false to enable/disable object storage (S3/minio). By default DB storage will be used.

When modifying the configuration setting using `PUT /user/settings/{id}`, ensure that the `value` property is a valid JSON object
and `_version` property is provided with the correct version number.


## 2. Configuration using `/users/configurations/entry` endpoints

#### Note: This method is not the preferred way of configuring Profile-Picture feature and starting from version 19.6.0 may not applicable anymore as the configuration entry which was used to be provided by this option will be migrated to the `users.settings` interface in version 19.6.0

### Permissions

###### users.configurations.item.put
###### users.configurations.item.get

### Invoke GET endpoint
#### Example request
`GET /users/configurations/entry`

### After GET, PUT endpoint needs to be invoked
#### Example request
`PUT /users/configurations/entry/{id}`

```json
{
  "id": "UUID of the configuration entry obtained from GET",
  "configName": "PROFILE_PICTURE_CONFIG",
  "enabled": true,
  "enabledObjectStorage": false,
  "encryptionKey": "fgrdvbfgjhutyrdhvbcxzmturdhgtiok",
  "maxFileSize": 4
  }
```

## Considerations for Profile-Picture Feature configuration

Note: `maxFileSize` must and should be within range of 0.1 to 10 megabytes.

By default DB storage will be enabled . To enable Object storage(S3/minio) below variables should be present in the env
`AWS_URL`
`AWS_REGION`
`AWS_ACCESS_KEY_ID`
`AWS_SECRET_ACCESS_KEY`

Note:- Bucket should pre-exist with same name as tenantName.
