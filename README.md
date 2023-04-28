jerk
=====

Turn JSON objects that conform with a JSON schema into erlang terms in
a uniform way. In addition to serialization and deserialization
functinos, Jerk presents an API for constructing new terms while
validating that input conforms to the schema.

Schema API
----------

The top level module provides the following API.

```erlang
-spec add_schema(Schema :: binary()) -> ok | {error, already_loaded}.
-spec load_schema(Path :: file:filename()) -> ok | {error, Reason}
              when Reason :: file:posix() | already_loaded.
-spec remove_schema(SchemaID :: schemaname()) -> ok | {error, not_loaded}.
```

Jerk terms
----------

Jerk represents terms with the opaque type `jerk:jerkterm()`.

Build
-----

    $ rebar3 compile
