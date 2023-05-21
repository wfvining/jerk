# jerk

Turn JSON objects that conform with a JSON schema into erlang terms in
a uniform way. In addition to serialization and deserialization
functinos, Jerk presents an API for constructing new terms while
validating that input conforms to the schema.

The top level module provides the following API.

## Managing the schema database

```erlang
%% @spec add_schema(Schema :: binary()) -> ok | {error, already_loaded}.

jerk:add_schema(<<"{\"$id\": \"foo\", \"type\": \"object\"}">>).
```

## Constructing terms

Jerk terms are represented by an opaque type `jerk:jerkterm/0` that
enables schema checking whenever a property is updated. Terms are
constructed from deep lists of `{Key, Value}` pairs. For example,
assume the schema below has been loaded into the schema database.

```json
{
  "$id": "urn:foo",
  "properties": {
    "a": {"type": "integer"},
    "b": {
      "type": "object",
      "properties": {
        "foo": {
          "type": "boolean"
        },
        "bar": {
          "type": "string",
          "enum": ["abc", "def"]
        }
      }
    }
  }
}
```

To construct the jerk term that conforms to the schema:

```erlang
T = jerk:new(
        <<"urn:foo">>,
        [{"a", 1},
         {"b", [{"foo", true},
                {"bar", "def"}]}]).
```

The values of properties can be retrieved using
`jerk:get_value/2`. Values that are "primitive" types (numbers,
booleans, strings, or lists/arrays) are returned directly as an erlang
term; however, values that are themselves objects are returned as a
jerk term and carry the schema information the object must comply
with.

```erlang
1 = jerk:get_value(T, <<"a">>).
<<"def">> = jerk:get_value(jerk:get_value(T, <<"b">>), <<"bar">>).
```
