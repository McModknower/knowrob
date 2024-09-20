MongoDB Knowledge Graph {#mongodb_backend}
============

A triple store implementation for the KnowRob knowledge base using MongoDB.

### Configuration

The MongoDB triple store can be configured via a property tree.
It has the following configuration parameters, all of which are optional:

| Key        | Type     | Description                                                              | Default Value |
|------------|----------|--------------------------------------------------------------------------|---------------|
| host       | string   | The host name.                                                           | localhost     |
| port       | string   | The port number.                                                         | 27017         |
| user       | string   | The username.                                                            | none          |
| password   | string   | The password.                                                            | none          |
| db         | string   | The database name.                                                       | knowrob       |
| collection | string   | The collection name.                                                     | triples       |
| read-only  | bool     | Whether the database is read-only.                                       | false         |
| drop       | [string] | List of named graphs to delete initially. "*" can be used to delete all. | []            |
