Redland Knowledge Graph {#redland_backend}
============

A triple store implementation for the KnowRob knowledge base using the Redland RDF library.
Note that only storage types that support context nodes can be used, as this is required
in order to store the "origin" of triples.
Additional contextual parameters (e.g. time, confidence, etc.) are not supported by this backend,
and thus need to be handled through reification (which is done automatically by KnowRob).

### Configuration

The Redland backend can be configured via a property tree.
It has the following configuration parameters, all of which are optional:

| Key      | Type   | Description                | Default Value |
|----------|--------|----------------------------|---------------|
| storage  | string | One of the memory types.   | `memory`      |
| host     | string | The host name.             | none          |
| port     | string | The port number.           | none          |
| user     | string | The username.              | none          |
| password | string | The password.              | none          |
| db       | string | The database name.         | none          |
| origin   | string | The origin of the triples. | none          |

The `storage` parameter can be one of the following values:
- `memory`: An in-memory storage that is not persistent.
- `hashes`: A storage that uses hash tables for indexing.
- `mysql`: A MySQL database.
- `postgresql`: A PostgreSQL database.
- `sqlite`: A SQLite database.
