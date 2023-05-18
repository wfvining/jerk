%%% @doc Public API for the Jerk application.
%%%
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright 2023 Will Vining <wfv@vining.dev>
-module(jerk).

%% Schema management API
-export([add_schema/1, load_schema/1, remove_schema/1]).

%% Term creation API
-export([new/2, attributes/1, get_value/2, set_value/3]).


-export_type([schemaname/0, jerkterm/0, primterm/0,
              attribute_name/0, attribute_value/0,
              type/0]).

-type schemaname() :: binary().

-type primterm() :: string:string() | number() | boolean() | null.

-type attribute_name() :: string:string().

-type attribute_value() :: jerkterm() | primterm().

-type type() :: object
              | integer
              | number
              | string
              | array
              | boolean
              | null
              | ref.

-opaque jerkterm() :: {URN :: binary(),
                       Attributes :: #{attribute_name() => attribute_value()}}.

%% @doc Add a schema to the Jerk schema catalog.
-spec add_schema(Schema :: string:string()) -> ok | {error, already_loaded}.
add_schema(Schema) ->
    Schemas = jerk_loader:load_json(Schema),
    lists:foreach(fun jerk_catalog:add_schema/1, Schemas).

%% @doc Load a schema from a file and add it to the Jerk schema catalog.
-spec load_schema(Path :: file:filename()) -> ok | {error, Reason}
              when Reason :: file:posix() | already_loaded.
load_schema(_Path) ->
    error(not_implemented).

%% @doc Remove a schema from the Jerk schema catalog.
-spec remove_schema(SchemaID :: schemaname()) -> ok.
remove_schema(_SchemaID) ->
    error(not_implemented).

%% @doc Create a new `JerkTerm' of the type specified by `SchemaId'. The
%% values of the attributes are given in `Attributes'. If any required
%% values are not in the attribute list or if any values do not
%% conform to the schema the call fails with reason `badarg'.
-spec new(SchemaId :: schemaname(),
          AttributeList :: [{attribute_name(), attribute_value()}]) ->
          jerkterm().
new(SchemaId, Attributes) ->
    Schema = jerk_catalog:get_schema(SchemaId),
    {SchemaId, load_attributes(Schema, Attributes)}.

load_attributes({SchemaId, _, _} = Schema, Attributes) ->
    load_attributes(SchemaId, Schema, Attributes).

load_attributes(_RootSchema, {_, object, ObjectDescription}, Attributes) ->
    Obj = maps:from_list(Attributes),
    case jerk_validator:validate(Obj, object, ObjectDescription) of
        true ->
            Obj;
        false ->
            error(badarg)
    end.


%% @doc Return the names of all defined attributes in `JerkTerm'.
-spec attributes(JerkTerm :: jerkterm()) -> [attribute_name()].
attributes(_JerkTerm) ->
    error(not_implemented).

%% @doc Return the value of `AttributeName'. If the attribute is not
%% defined the call fails with reason `badarg'.
-spec get_value(JerkTerm :: jerkterm(),
                AttributeName :: attribute_name()) -> attribute_value().
get_value(_, _) ->
    error(not_implemented).

%% @doc Set the value of `Attribute' to `Value' in `JerkTerm'. If the
%% attribute is not allowed in schema of `JerkTerm' or if the value is
%% not allowed the call fails with reason `badarg'.
-spec set_value(JerkTerm,
                Attribute :: attribute_name(),
                Value :: attribute_value()) ->
          JerkTerm when JerkTerm :: jerkterm().
set_value(JerkTerm, AttributeName, Value) ->
    error(not_implemented).
