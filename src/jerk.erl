%%% @doc Public API for the Jerk application.
%%%
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright 2023 Will Vining <wfv@vining.dev>
-module(jerk).

%% Schema management API
-export([add_schema/1, load_schema/1, remove_schema/1]).

-export([new/2, id/1, attributes/1, get_value/2, set_value/3,
         is_object/1, is_primitive/1]).


-export_type([schemaname/0, object/0, primitive/0,
              attribute_name/0, attribute_value/0,
              type/0]).

-type schemaname() :: binary().

-type primitive() :: string:string() | number() | boolean() | null.

-type attribute_name() :: string:string().

-type attribute_value() :: object() | primitive().

-type type() :: object
              | integer
              | number
              | string
              | array
              | boolean
              | null
              | ref.

-opaque object() :: {Id :: binary(),
                     Attributes :: #{attribute_name() => attribute_value()}}.

%% @doc Add a schema to the Jerk schema catalog.
-spec add_schema(Schema :: string:string()) -> ok | {error, already_loaded}.
add_schema(Schema) ->
    Schemas = jerk_loader:load_json(Schema),
    lists:foreach(fun jerk_catalog:add_schema/1, Schemas).

%% @doc Load a schema from a file and add it to the Jerk schema catalog.
-spec load_schema(Path :: file:filename()) -> ok | {error, Reason}
              when Reason :: file:posix() | already_loaded.
load_schema(Path) ->
    case file:read_file(Path) of
        {ok, Schema} ->
            add_schema(Schema);
        {error, _} = Error ->
            Error
    end.

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
          object.
new(SchemaId, Attributes) ->
    Schema = jerk_catalog:get_schema(SchemaId),
    make_term(SchemaId, Schema, Attributes).

make_term(BaseURI, {SchemaId, object, ObjectDescription}, Attributes) ->
    make_object(BaseURI, SchemaId, ObjectDescription, Attributes);
make_term(BaseURI, {_Id, ref, Path}, Value) ->
    %% TODO differentiate between absolute and relative URIs
    URI = <<BaseURI/binary, Path/binary>>,
    make_term(BaseURI, jerk_catalog:get_schema(URI), Value);
make_term(BaseURI, {Id, array, Constraints} = Description, Value) ->
    Arr = case lists:keyfind(allowed, 1, Constraints) of
              {allowed, ElementDescription} ->
                  Schema = jerk_loader:from_anonymous(
                             make_uri(BaseURI, <<"#/properties/", Id/binary, "/$array-item">>),
                             ElementDescription),
                  [make_array_element(BaseURI, Schema, V)
                   || V <- Value];
              false ->
                  Value
          end,
    case validate(BaseURI, Description, Arr) of
        true -> Arr;
        false -> error(badarg)
    end;
make_term(BaseURI, Description, Value) ->
    case validate(BaseURI, Description, Value) of
        true -> Value;
        false -> error(badarg)
    end.

make_array_element(BaseURI, Schema, Value) ->
    case make_term(BaseURI, Schema, Value) of
        {_, X} -> X;
        X -> X
    end.

make_object(BaseURI, SchemaId, {Properties, Required, Frozen}, Attributes) ->
    O = lists:foldl(
          fun ({Name, Value}, Obj) ->
                  case lists:keyfind(Name, 1, Properties) of
                      false when Frozen -> error(badarg);
                      false when not Frozen ->
                          Obj#{Name => maybe_object(Value)};
                      {Name, object, Description} when is_list(Value) ->
                          {_, Term} =
                              make_term(
                                BaseURI,
                                %% use a JSON pointer to refer to the anonymous
                                %% subschema within the properties of SchemaId.
                                {<<SchemaId/binary, "#/properties/", Name/binary>>,
                                 object, Description},
                                Value),
                          Obj#{Name => Term};
                      Description
                        when is_tuple(Value) andalso tuple_size(Value) =:= 2 ->
                          case validate(BaseURI, Description, element(2, Value)) of
                              true ->
                                  Obj#{Name => element(2, Value)};
                              false ->
                                  error(badarg)
                          end;
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
-spec id(Term :: object) -> binary().
id({ID, _}) ->
    ID.

%% @doc Return the names of all defined attributes in `JerkTerm'.
-spec attributes(JerkTerm :: object) -> [attribute_name()].
attributes({_, Attributes}) ->
    maps:keys(Attributes).

%% @doc Return the value of `AttributeName'. If the attribute is not
%% defined the call fails with reason `badarg'.
-spec get_value(JerkTerm :: object,
                AttributeName :: attribute_name()) -> attribute_value().
get_value({SchemaId, Attributes}, AttributeName) ->
    try
        Value = maps:get(AttributeName, Attributes),
        maybe_term(SchemaId, AttributeName, Value)
    catch error:{badkey, AttributeName} ->
            error({undefined, AttributeName})
    end.

maybe_term(SchemaId, PropertyName, Value) when is_list(Value) ->
    PropertyPrefix =
        case lists:reverse(binary:split(SchemaId, <<"/">>, [global])) of
            [<<"$array-item">>|_] ->
                <<"/properties/">>;
            _ ->
                <<"#/properties/">>
        end,
    [if is_map(V) ->
             {make_uri(SchemaId, <<PropertyPrefix/binary, PropertyName/binary, "/$array-item">>), V};
        true -> V
     end || V <- Value];
maybe_term(SchemaId, PropertyName, Value) when is_map(Value) ->
    case lists:reverse(binary:split(SchemaId, <<"/">>, [global])) of
        [<<"$array-item">>|_] ->
            case jerk_catalog:get_schema(SchemaId) of
                {_, ref, Path} ->
                    maybe_term(make_uri(SchemaId, Path), PropertyName, Value);
                {_, object, {Properties, _, _}} ->
                    case lists:keyfind(PropertyName, 1, Properties) of
                        false -> Value;
                        {_, ref, Path} -> {make_uri(SchemaId, Path), Value};
                        {_, _, _} -> {make_uri(SchemaId, <<"/properties/", PropertyName/binary>>)}
                    end
            end;
        _ ->
            {_, object, {Properties, _, _}} = jerk_catalog:get_schema(SchemaId),
            case lists:keyfind(PropertyName, 1, Properties) of
                false ->
                    Value;
                {_, ref, Path} ->
                    {make_uri(SchemaId, Path), Value};
                {_, _, _} ->
                    {make_uri(SchemaId, <<"#/properties/", PropertyName/binary>>),
                     Value}
            end
    end;
maybe_term(_, _, Value) ->
    Value.

combine_uri(URI, <<"#/properties", PropertyName/binary>> = Path) ->
    case binary:split(URI, <<"#/">>) of
        [URI] ->
            <<URI/binary, Path/binary>>;
        [BaseURI, <<"properties", _/binary>>] ->
            <<BaseURI/binary, Path/binary>>;
        [_BaseURI, _ExtPath] ->
            <<URI/binary, "/properties", PropertyName/binary>>
    end;
combine_uri(URI, <<"#/", _/binary>> = Path) ->
    [BaseURI|_] = binary:split(URI, <<"#/">>),
    <<BaseURI/binary, Path/binary>>;
combine_uri(URI, Path) ->
    <<URI/binary, Path/binary>>.

make_uri(URI, Path) ->
    combine_uri(URI, Path).
%% make_uri(URI, <<"#/", _/binary>> = Path) ->
%%     [BaseURI|_] = string:split(URI, <<"#/">>),
%%     <<BaseURI/binary, Path/binary>>;
%% make_uri(URI, Path) ->
%%     <<URI/binary, Path/binary>>.

%% @doc Set the value of `Attribute' to `Value' in `JerkTerm'. If the
%% attribute is not allowed in schema of `JerkTerm' or if the value is
%% not allowed the call fails with reason `badvalue'.
-spec set_value(JerkTerm,
                Attribute :: attribute_name(),
                Value :: attribute_value()) ->
          JerkTerm when JerkTerm :: object.
set_value({ID, Obj}, AttributeName, Value) ->
    Value1 = prep_value(Value),
    NewObj =
        Obj#{AttributeName => Value1},
    Schema = jerk_catalog:get_schema(ID),
    case validate(ID, Schema, NewObj) of
        true -> {ID, NewObj};
        false -> error(badvalue)
    end.

prep_value(Value) when is_list(Value) ->
    [prep_value(V) || V <- Value];
prep_value(Value) when is_tuple(Value), tuple_size(Value) =:= 2 ->
    element(2, Value);
prep_value(Value) when is_tuple(Value) ->
    error(badarg);
prep_value(Value) ->
    Value.

is_primitive(X) ->
    is_list(X)
        orelse is_number(X)
        orelse is_boolean(X)
        orelse is_binary(X)
        orelse X =:= null.

is_object({URI, Map}) when is_map(Map), is_binary(URI) ->
    try jerk_catalog:get_schema(URI) of
        _ -> true
    catch
        error:badarg -> false
    end;
is_object(_) -> false.
