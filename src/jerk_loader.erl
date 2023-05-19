-module(jerk_loader).

-export([load_json/1]).

%% @doc Load a schema from a JSON string.
-spec load_json(Schema :: string:string()) -> [{binary(), jerk:type(), any()}].
load_json(Schema) ->
    JSONTerm = jiffy:decode(Schema, [return_maps]),
    load(JSONTerm, []).

load(#{<<"$id">> := SchemaID} = Schema, Schemas) ->
    load_definition(SchemaID, Schema, Schemas).

load_object(ID, Object, Schemas) ->
    Properties =
        load_properties(
          maps:get(<<"properties">>, Object, #{})),
    SubTypes =
        load_definitions(
          <<ID/binary, "#/definitions">>,
          maps:get(<<"definitions">>, Object, #{}),
          []),
    Required = maps:get(<<"required">>, Object, []),
    Frozen = not maps:get(<<"additionalProperties">>, Object, true),
    {{Properties, Required, Frozen}, SubTypes ++ Schemas}.

load_properties(Properties) ->
    maps:fold(fun load_definition/3, [], Properties).

length_constraint(Sense, Value) when is_integer(Value), Value >= 0 ->
    {length, {Sense, Value}}.

load_length_constraints(Object) ->
    Min = maps:get(<<"minLength">>, Object, undefined),
    Max = maps:get(<<"maxLength">>, Object, undefined),
    [length_constraint(Sense, Value)
     || {Sense, Value} <- [{min, Min}, {max, Max}], Value =/= undefined].

load_enum_constraint(#{<<"enum">> := Enum}) when is_list(Enum) ->
    [jerk_constraint:enum(Enum)];
load_enum_constraint(_) ->
    [].

string_constraints(Object) ->
    lists:flatten(
      [load_length_constraints(Object), load_enum_constraint(Object)]).

load_ub_constraint(#{<<"maximum">> := Max}) ->
    [jerk_constraint:range(ub, inclusive, Max)];
load_ub_constraint(#{<<"exclusiveMaximum">> := Max}) ->
    [jerk_constraint:range(ub, exclusive, Max)];
load_ub_constraint(_) ->
    [].

load_lb_constraint(#{<<"minimum">> := Min}) ->
    [jerk_constraint:range(lb, inclusive, Min)];
load_lb_constraint(#{<<"exclusiveMinimum">> := Min}) ->
    [jerk_constraint:range(lb, exclusive, Min)];
load_lb_constraint(_) ->
    [].

load_range_constraints(Object) ->
    UB = load_ub_constraint(Object),
    LB = load_lb_constraint(Object),
    UB ++ LB.

load_multiples_constraints(#{<<"multipleOf">> := Divisor}) ->
    [jerk_constraint:multiple(Divisor)];
load_multiples_constraints(_) ->
    [].

number_constraints(Object) ->
    lists:flatten(
      [load_range_constraints(Object), load_multiples_constraints(Object)]).

load_items_constraints(Object) ->
    Max = maps:get(<<"maxItems">>, Object, undefined),
    Min = maps:get(<<"minItems">>, Object, undefined),
    Allowed = case maps:get(<<"items">>, Object, undefined) of
                  undefined -> [];
                  ItemSchema ->
                      [{<<"">>, Type, Constraints}] =
                          load_definition(<<"">>, ItemSchema, []),
                      [jerk_constraint:items(Type, Constraints)]
              end,
    [jerk_constraint:item_count(Sense, Value)
     || {Sense, Value} <- [{min, Min}, {max, Max}], Value =/= undefined]
        ++ Allowed.

load_unique_constraint(#{<<"uniqueItems">> := true}) ->
    [jerk_constraint:unique()];
load_unique_constraint(_) ->
    [].

contains_count_constraint(Sense, Value)
  when is_integer(Value) andalso Value >= 0->
    {count, {Sense, Value}}.

load_contains_constraints(#{<<"contains">> := Schema}) ->
    Contains = load_definition(<<"contained">>, Schema, []),
    MinContains = maps:get(<<"minContains">>, Schema, undefined),
    MaxContains = maps:get(<<"maxContains">>, Schema, undefined),
    MinCountConstraint =
        if MinContains =/= undefined ->
                [contains_count_constraint(min, MinContains)];
            true ->
                []
        end,
    MaxCountConstraint =
        if MaxContains =/= undefined ->
                [contains_count_constraint(max, MaxContains)];
           true ->
                []
        end,
    ContainsConstraint = jerk_constraint:contains(Contains),
    [ContainsConstraint | MinCountConstraint ++ MaxCountConstraint];
load_contains_constraints(_) ->
    [].

array_constraints(Object) ->
    Items = load_items_constraints(Object),
    Contains = load_contains_constraints(Object),
    Unique = load_unique_constraint(Object),
    Items ++ Contains ++ Unique.

load_definition(ID, #{<<"type">> := <<"object">>} = Object, Schemas)  ->
    {ObjectDesctription, NewSchemas} = load_object(ID, Object, Schemas),
    [new_record(object, ID, ObjectDesctription) | NewSchemas];
load_definition(ID, #{<<"type">> := <<"string">>} = Object, Schemas) ->
    [new_record(string, ID, string_constraints(Object)) | Schemas];
load_definition(ID, #{<<"type">> := <<"number">>} = Object, Schemas) ->
    [new_record(number, ID, number_constraints(Object)) | Schemas];
load_definition(ID, #{<<"type">> := <<"integer">>} = Object, Schemas) ->
    [new_record(integer, ID, number_constraints(Object)) | Schemas];
load_definition(ID, #{<<"type">> := <<"boolean">>}, Schemas) ->
    [new_record(boolean, ID, []) | Schemas];
load_definition(ID, #{<<"type">> := <<"array">>} = Object, Schemas) ->
    [new_record(array, ID, array_constraints(Object)) | Schemas];
load_definition(ID, #{<<"type">> := <<"null">>}, Schemas) ->
    [new_record(null, ID, []) | Schemas];
load_definition(_, #{<<"type">> := _}, _) ->
    error(badarg);
load_definition(ID, #{<<"$ref">> := Reference}, Schemas)
  when is_binary(Reference) ->
    [new_record(ref, ID, Reference) | Schemas];
load_definition(ID, _, Schemas) ->
    [new_record(any, ID, nil) | Schemas].


load_definitions(BaseID, Definitions, Schemas) ->
    maps:fold(
      fun (ID, Value, Acc) ->
              load_definition(<<BaseID/binary, "/", ID/binary>>, Value, Acc)
      end,
      Schemas, Definitions).

new_record(Type, ID, Description) ->
    {ID, Type, Description}.
