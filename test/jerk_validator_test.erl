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
                          [MaxLenConstraint, RefConstraint]]]}].

%% TODO Validation of a constraint with a reference to another schema
%%      returns a continuation.

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