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
To enable this feature for a tenant, we need to perform below operations
### Permissions

###### users.configurations.item.put
###### users.configurations.item.get

### Invoke GET endpoint
#### Example request
GET https://{okapi-location}/users/configurations/entry

### After GET, PUT endpoint needs to be invoked
#### Example request
PUT https://{okapi-location}/users/configurations/entry/{id}

{
"id": {{id}},
"configName": "PROFILE_PICTURE_CONFIG",
"enabled": true,
"enabledObjectStorage": false,
"encryptionKey": "fgrdvbfgjhutyrdhvbcxzmturdhgtiok",
"maxFileSize": 4
}

Note: maxFileSize must and should be within range of 0.1 to 10 megabytes.

By default DB storage will be enabled . To enable Object storage(S3/minio) below variables should be present in the env
AWS_URL
AWS_REGION
AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY

Note:- Bucket should pre-exist with same name as tenantName.

#### Example request
PUT https://{okapi-location}/users/configurations/entry/{id}

{
"id": {{id}},
"configName": "PROFILE_PICTURE_CONFIG",
"enabled": true,
"enabledObjectStorage": true,
"encryptionKey": "fgrdvbfgjhutyrdhvbcxzmturdhgtiok",
"maxFileSize": 4
}
