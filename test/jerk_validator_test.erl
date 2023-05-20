-module(jerk_validator_test).

-include_lib("eunit/include/eunit.hrl").

validate_primitive_type_test_() ->
    Valid = [{1, integer},
             {1, number},
             {1.0, number},
             {null, null},
             {true, boolean},
             {[1, 2], array},
             {<<"string">>, string}],
    {Values, Types} = lists:unzip(Valid),
    {inparallel,
     [?_assertEqual(
         lists:member({X, Type}, Valid),
         jerk_validator:validate(X, Type, []))
      || X <- Values, Type <- Types]}.

validate_with_constraint_test_() ->
    {inparallel,
     [number_constraint()]}.

number_constraint() ->
    Constraints = [jerk_constraint:range(lb, inclusive, -1),
                   jerk_constraint:range(ub, inclusive, 3)],
    [{inparallel,
      [?_assert(jerk_validator:validate(
                  X, number, Constraints))
       || X <- [-1, 0, 3]]},
     {inparallel,
      [?_assert(not jerk_validator:validate(
                      X, number, Constraints))
       || X <- [-1.1, 3.5]]}].

validate_items_object_test_() ->
    Array = [#{<<"foo">> => 1}, #{<<"foo">> => 2}],
    DirectConstraint =
        jerk_constraint:items(
          object,
          {[{<<"foo">>, integer, [jerk_constraint:range(ub, inclusive, 2)]}],
            [<<"foo">>], true}),
    RefConstraint =
        jerk_constraint:items(ref, <<"urn:foo">>),
    MaxLenConstraint =
        jerk_constraint:item_count(max, 1),
    {inparallel,
     [{"validation of items constraint with locally defined object type succeeds",
       ?_assert(jerk_validator:validate(Array, array, [DirectConstraint]))},
      {"validation of items constraint with a reference results in a continuation",
       ?_assertEqual(
          {continue, [{#{<<"foo">> => 1}, {ref, <<"urn:foo">>}},
                      {#{<<"foo">> => 2}, {ref, <<"urn:foo">>}}]},
          jerk_validator:validate(Array, array, [RefConstraint]))},
      {"validation of items constraint fails if "
       "length constraints are not satisfied",
       [?_assert(not jerk_validator:validate(
                       Array, array, Constraints))
        || Constraints <- [[RefConstraint, MaxLenConstraint],
                           [MaxLenConstraint, RefConstraint]]]}]}.

validate_items_primitive_test_() ->
    GoodArray = [1, -1],
    BadArray = [1, 2],
    Constraint =
        jerk_constraint:items(
          integer, [jerk_constraint:range(lb, inclusive, -1),
                    jerk_constraint:range(ub, inclusive, 1)]),
    {inparallel,
     [?_assert(jerk_validator:validate(GoodArray, array, [Constraint])),
      ?_assert(not jerk_validator:validate(BadArray, array, [Constraint])),
      ?_assert(not jerk_validator:validate(
                     [1|GoodArray], array,
                     [Constraint, jerk_constraint:item_count(max, 2)]))]}.

validate_ref_test() ->
    Bar = #{<<"bar">> => 2},
    Obj = #{<<"foo">> => Bar},
    ObjDescription = {[{<<"foo">>, ref, <<"#/bar">>}],
                      [<<"foo">>],
                      true},
    ?assertEqual({continue, [{Bar, {ref, <<"#/bar">>}}]},
                 jerk_validator:validate(Obj, object, ObjDescription)).

bad_type_test() ->
    ?assert(
       not jerk_validator:validate(
             3, string, [jerk_constraint:length(min, 1)])).

validate_object_test() ->
    Obj = #{<<"a">> => <<"foo">>, <<"b">> => 1},
    {_If, Type, Description} =
        {<<"obj">>, object,
         {[{<<"a">>, string, [jerk_constraint:length(min, 2),
                              jerk_constraint:length(max, 3)]},
           {<<"b">>, integer, []}],
          [<<"a">>, <<"b">>],
          false}},
    ?assert(jerk_validator:validate(Obj, Type, Description)).

validate_any_type_test_() ->
    {inparallel,
     [?_assert(jerk_validator:validate(X, any, nil))
      || X <- [1, 12.1, <<"foo">>, false, null,
               #{<<"complex">> => #{<<"nested">> => <<"stuff">>}}]]}.

complex_nested_test() ->
    SchemaBar =
        jiffy:encode(
          #{<<"$id">> => <<"bar">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"a">> => #{<<"$ref">> => <<"#/definitions/TypeA">>},
                  <<"b">> => #{<<"type">> => <<"integer">>},
                  <<"c">> => #{<<"type">> => <<"object">>,
                               <<"properties">> => #{<<"d">> => <<"integer">>}}},
            <<"required">> => [<<"a">>],
            <<"additionalProperties">> => false}),
    [{<<"bar">>, object, ObjDescription}] = jerk_loader:load_json(SchemaBar),
    ValidationResult =
        jerk_validator:validate(
          #{<<"a">> => #{<<"name">> => <<"foo">>},
            <<"b">> => 1,
            <<"c">> => #{<<"d">> => 2}},
          object,
          ObjDescription),
    ?assertEqual(
       {continue, [{#{<<"name">> => <<"foo">>},
                    {ref, <<"#/definitions/TypeA">>}}]},
        ValidationResult).

multiple_continuation_test() ->
    SchemaBar =
        jiffy:encode(
          #{<<"$id">> => <<"bar">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"a">> => #{<<"$ref">> => <<"#/definitions/TypeA">>},
                  <<"b">> => #{<<"type">> => <<"integer">>},
                  <<"c">> => #{<<"$ref">> => <<"#/definitions/TypeC">>}},
            <<"required">> => [<<"a">>],
            <<"additionalProperties">> => false}),
    [{<<"bar">>, object, ObjDescription}] = jerk_loader:load_json(SchemaBar),
    {continue, Cont} =
        jerk_validator:validate(
          #{<<"a">> => #{<<"name">> => <<"foo">>},
            <<"b">> => 1,
            <<"c">> => #{<<"d">> => 2}},
          object,
          ObjDescription),
    ?assertEqual(
       lists:sort(
         [{#{<<"name">> => <<"foo">>}, {ref, <<"#/definitions/TypeA">>}},
          {#{<<"d">> => 2}, {ref, <<"#/definitions/TypeC">>}}]),
       lists:sort(Cont)).
