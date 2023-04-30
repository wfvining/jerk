-module(jerk_loader).

-export([load_json/1]).

load_json(Schema) ->
    JSONTerm = jiffy:decode(Schema, [return_maps]),
    load(JSONTerm, []).

load(#{<<"$id">> := SchemaID, <<"type">> := <<"object">>} = Schema, Schemas) ->
    % properties, additionalProperties, required
    {ObjectDesctription, NewSchemas} =
        load_object(SchemaID, Schema, Schemas),
    [new_record(object, SchemaID, ObjectDesctription) | NewSchemas].

load_object(ID, Object, Schemas) ->
    Properties =
        load_properties(
          maps:get(<<"properties">>, Object, #{})),
    SubTypes =
        load_definitions(
          <<ID/binary, "/definitions">>,
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

enum_constraint(Values) ->
    {enum, Values}.

load_enum_constraint(#{<<"enum">> := Enum}) when is_list(Enum) ->
    [enum_constraint(Enum)];
load_enum_constraint(_) ->
    [].

string_constraints(Object) ->
    lists:flatten(
      [load_length_constraints(Object), load_enum_constraint(Object)]).

range_constraint(UbLb, Exclusive, N) when is_number(N) ->
    {UbLb, {Exclusive, N}};
range_constraint(_, _, _) ->
    error(badarg).

load_ub_constraint(#{<<"maximum">> := Max}) ->
    [range_constraint(ub, inclusive, Max)];
load_ub_constraint(#{<<"exclusiveMaximum">> := Max}) ->
    [range_constraint(ub, exclusive, Max)];
load_ub_constraint(_) ->
    [].

load_lb_constraint(#{<<"minimum">> := Min}) ->
    [range_constraint(lb, inclusive, Min)];
load_lb_constraint(#{<<"exclusiveMinimum">> := Min}) ->
    [range_constraint(lb, exclusive, Min)];
load_lb_constraint(_) ->
    [].

load_range_constraints(Object) ->
    UB = load_ub_constraint(Object),
    LB = load_lb_constraint(Object),
    UB ++ LB.

multiple_constraint(Divisor) when is_number(Divisor) ->
    {multipleof, Divisor}.

load_multiples_constraints(#{<<"multipleOf">> := Divisor}) ->
    [multiple_constraint(Divisor)];
load_multiples_constraints(_) ->
    [].

number_constraints(Object) ->
    lists:flatten(
      [load_range_constraints(Object), load_multiples_constraints(Object)]).

items_constraint(Sense, Value)
  when is_integer(Value) andalso Value >= 0 ->
    {items, {Sense, Value}}.

load_items_constraints(Object) ->
    Max = maps:get(<<"maxItems">>, Object, undefined),
    Min = maps:get(<<"minItems">>, Object, undefined),
    [items_constraint(Sense, Value) ||
        {Sense, Value} <- [{min, Min}, {max, Max}], Value =/= undefined].

load_unique_constraint(#{<<"uniqueItems">> := true}) ->
    [{unique, true}];
load_unique_constraint(_) ->
    [].

contains_count_constraint(Sense, Value)
  when is_integer(Value) andalso Value >= 0->
    {count, {Sense, Value}}.

contains_constraint(Definition) ->
    {contains, Definition}.

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
    ContainsConstraint = contains_constraint(Contains),
    [ContainsConstraint | MinCountConstraint ++ MaxCountConstraint];
load_contains_constraints(_) ->
    [].

array_constraints(Object) ->
    ArrayLength = load_items_constraints(Object),
    Contains = load_contains_constraints(Object),
    Unique = load_unique_constraint(Object),
    ArrayLength ++ Contains ++ Unique.

load_definition(ID, #{<<"type">> := <<"object">>} = Object, Schemas)  ->
    {ObjectDesctription, NewSchemas} = load_object(ID, Object, Schemas),
    [new_record(object, ID, ObjectDesctription) | NewSchemas];
load_definition(ID, Object, Schemas) ->
    Record =
        case maps:get(<<"type">>, Object, notype) of
            <<"string">> -> new_record(string, ID, string_constraints(Object));
            <<"number">> -> new_record(number, ID, number_constraints(Object));
            <<"integer">> -> new_record(integer, ID, number_constraints(Object));
            <<"boolean">> -> new_record(boolean, ID, []);
            <<"array">> -> new_record(array, ID, array_constraints(Object));
            <<"null">> -> new_record(null, ID, [])
        end,
    [Record | Schemas].

load_definitions(BaseID, Definitions, Schemas) ->
    maps:fold(
      fun (ID, Value, Acc) ->
              load_definition(<<BaseID/binary, ID/binary>>, Value, Acc)
      end,
      Schemas, Definitions).

new_record(Type, ID, Description) ->
    {ID, Type, Description}.
