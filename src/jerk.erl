%%% @doc Public API for the Jerk application.
%%%
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright 2023 Will Vining <wfv@vining.dev>
-module(jerk).

%% Schema management API
-export([add_schema/1, load_schema/1, remove_schema/1]).

-export([new/2, id/1, attributes/1, get_value/2, set_value/3]).


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

-opaque jerkterm() :: {Id :: binary(),
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
    make_term(SchemaId, Schema, Attributes).

make_term(BaseURI, {SchemaId, object, ObjectDescription}, Attributes) ->
    make_object(BaseURI, SchemaId, ObjectDescription, Attributes);
make_term(BaseURI, {_Id, ref, Path}, Value) ->
    %% TODO differentiate between absolute and relative URIs
    URI = <<BaseURI/binary, Path/binary>>,
    make_term(BaseURI, jerk_catalog:get_schema(URI), Value);
make_term(BaseURI, Description, Value) ->
    case validate(BaseURI, Description, Value) of
        true -> Value;
        false -> error(badarg)
    end.

make_object(BaseURI, SchemaId, {Properties, Required, Frozen}, Attributes) ->
    O = lists:foldl(
          fun ({Name, Value}, Obj) ->
                  case lists:keyfind(Name, 1, Properties) of
                      false when Frozen -> error(badarg);
                      false when not Frozen ->
                          Obj#{Name => maybe_object(Value)};
                      {Name, object, Description} ->
                          {_, Term} =
                              make_term(
                                BaseURI,
                                %% use a JSON pointer to refer to the anonymous
                                %% subschema within the properties of SchemaId.
                                {<<SchemaId/binary, "#/properties/", Name/binary>>,
                                 object, Description},
                                Value),
                          Obj#{Name => Term};
                      Description ->
                          case make_term(BaseURI, Description, Value) of
                              {_, Term} ->
                                  Obj#{Name => Term};
                              Term ->
                                  Obj#{Name => Term}
                          end
                  end
          end,
          #{},
          Attributes),
    case lists:all(fun(RequiredKey) -> maps:is_key(RequiredKey, O) end,
                   Required)
    of
        true ->
            {SchemaId, O};
        false ->
            error(badarg)
    end.

validate(URI, {_, Type, ObjectDescription}, Obj) ->
    case jerk_validator:validate(Obj, Type, ObjectDescription) of
        {continue, Cont} ->
            lists:all(
              fun ({X, {ref, Path}}) ->
                      Schema =
                          jerk_catalog:get_schema(make_uri(URI, Path)),
                      validate(URI, Schema, X);
                  ({X, {XType, Description}}) ->
                      validate(URI, {'anonymous', XType, Description}, X)
              end, Cont);
        Result -> Result
    end.

maybe_object([{_, _}|_] = X) ->
    from_list(X);
maybe_object(X) ->
    X.

from_list(AttributeList) ->
    lists:foldl(
      fun ({Name, [{_, _}|_] = Value}, Map) ->
              maps:put(Name, from_list(Value), Map);
          ({Name, Value}, Map) ->
              maps:put(Name, Value, Map)
      end,
      #{},
      AttributeList).

%% @doc Return the identifier of the schema for `Term'
-spec id(Term :: jerkterm()) -> binary().
id({ID, _}) ->
    ID.

%% @doc Return the names of all defined attributes in `JerkTerm'.
-spec attributes(JerkTerm :: jerkterm()) -> [attribute_name()].
attributes({_, Attributes}) ->
    maps:keys(Attributes).

%% @doc Return the value of `AttributeName'. If the attribute is not
%% defined the call fails with reason `badarg'.
-spec get_value(JerkTerm :: jerkterm(),
                AttributeName :: attribute_name()) -> attribute_value().
get_value({SchemaId, Attributes}, AttributeName) ->
    try
        Value = maps:get(AttributeName, Attributes),
        maybe_term(SchemaId, AttributeName, Value)
    catch error:{badkey, AttributeName} ->
            error({undefined, AttributeName})
    end.

maybe_term(SchemaId, PropertyName, Value) when is_map(Value) ->
    {_, object, {Properties, _, _}} = jerk_catalog:get_schema(SchemaId),
    case lists:keyfind(PropertyName, 1, Properties) of
        false ->
            Value;
        {_, ref, Path} ->
            {make_uri(SchemaId, Path), Value};
        {_, _, _} ->
            {make_uri(SchemaId, <<"#/properties/", PropertyName/binary>>),
             Value}
    end;
maybe_term(_, _, Value) ->
    Value.

make_uri(URI, <<"#/", _/binary>> = Path) ->
    [BaseURI|_] = string:split(URI, <<"#/">>),
    <<BaseURI/binary, Path/binary>>;
make_uri(URI, Path) ->
    <<URI/binary, Path/binary>>.

%% @doc Set the value of `Attribute' to `Value' in `JerkTerm'. If the
%% attribute is not allowed in schema of `JerkTerm' or if the value is
%% not allowed the call fails with reason `badarg'.
-spec set_value(JerkTerm,
                Attribute :: attribute_name(),
                Value :: attribute_value()) ->
          JerkTerm when JerkTerm :: jerkterm().
set_value({ID, Obj}, AttributeName, Value) ->
    NewObj =
        Obj#{AttributeName =>
                 if is_tuple(Value) andalso tuple_size(Value) =:= 2 ->
                         element(2, Value);
                    is_tuple(Value) -> error(badarg);
                    true -> Value end},
    Schema = jerk_catalog:get_schema(ID),
    case validate(ID, Schema, NewObj) of
        true -> {ID, NewObj};
        false -> error(badvalue)
    end.
