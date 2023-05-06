-module(jerk_constraint_test).

-include_lib("eunit/include/eunit.hrl").

number_range_constraint_test_() ->
    {"test a number above equal-to or below the value of "
     "inclusive and exclusive upper and lower bound constraints",
     {inparallel,
      [lists:zipwith(
         fun(Constraint, {Expected, TestValue}) ->
                 ?_assertEqual(Expected,
                               jerk_constraint:validate(Constraint, TestValue))
         end,
         lists:duplicate(3, jerk_constraint:range(UbLb, Exclusive, N)),
         [{Exclusive =/= exclusive, N}, %% result when constraint is applied to ?N
          {UbLb =:= ub, N - 1}, %% result when applied to a number less than ?N
          {UbLb =:= lb, N + 1}]) %% result when applied to a number greater than ?N
       || UbLb <- [ub, lb],
          Exclusive <- [exclusive, inclusive],
          N <- [10, 10.5, -10, -10.5]]}}.

number_multipleof_constraint_test_() ->
    {"test that multiple of works for integers and floats",
     {inparallel,
      [[?_assert(jerk_constraint:validate(
                   jerk_constraint:multiple(0), Zero))
        || Zero <- [0, 0.0]],
       [?_assert(not jerk_constraint:validate(
                       jerk_constraint:multiple(0), NotZero))
        || NotZero <- [1, -1, -1.2, 0.1]],
       [?_assert(jerk_constraint:validate(
                   jerk_constraint:multiple(3), X))
        || X <- [-3, -21, 9]],
       [?_assert(not jerk_constraint:validate(
                       jerk_constraint:multiple(3), X))
        || X <- [1, 2, 22, -4]],
       [?_assert(jerk_constraint:validate(
                   jerk_constraint:multiple(1.5), X * 1.5))
        || X <- [-3, -21, 9]],
       [?_assert(not jerk_constraint:validate(
                       jerk_constraint:multiple(3.2), X * 2.3))
        || X <- [-3, -21, 9]],
       [?_assert(jerk_constraint:validate(
                   jerk_constraint:multiple(3.0), 9)),
        ?_assert(jerk_constraint:validate(
                   jerk_constraint:multiple(3), 9.0)),
        ?_assert(not jerk_constraint:validate(
                       jerk_constraint:multiple(3), 9.1)),
        ?_assert(not jerk_constraint:validate(
                       jerk_constraint:multiple(3.1), 9))]]}}.

enum_constraint_test_() ->
    ValidValues = [<<"Foo">>, <<"Bar">>, <<"Baz">>],
    Enum = jerk_constraint:enum(ValidValues),
    [{"values in the enum list satisfy the constraint",
      {inparallel,
       [?_test(?assert(jerk_constraint:validate(
                         Enum, Value)))
        || Value <- ValidValues]}},
     {"values not in the enum fail to satisfy the constraint",
      {inparallel,
       [?_test(?assert(not jerk_constraint:validate(
                             Enum, Value)))
        || Value <- [<<"Foo1">>, 1, null, false, true, <<"">>, 1.2]]}}].

items_constraint_test_() ->
    MaxItemsConstraint = jerk_constraint:items(max, 3),
    MinItemsConstraint = jerk_constraint:items(min, 1),
    [{"maximum items constraints",
      [?_test(
          ?assertEqual(length(L) =< 3,
                       jerk_constraint:validate(MaxItemsConstraint, L)))
       || L <- [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]]},
     {"minimum items constraints",
      [?_test(
          ?assertEqual(length(L) >= 1,
                       jerk_constraint:validate(MinItemsConstraint, L)))
       || L <- [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]]}].

unique_constraint_test_() ->
    Unique = jerk_constraint:unique(),
    {inparallel,
     [?_test(?assert(jerk_constraint:validate(Unique, []))),
      ?_test(?assert(jerk_constraint:validate(Unique, [1]))),
      ?_test(?assert(jerk_constraint:validate(Unique, [1, 2]))),
      ?_test(?assert(jerk_constraint:validate(Unique, [1, 2.3, <<"foo">>]))),
      ?_test(?assert(jerk_constraint:validate(Unique, [<<"foo">>, <<"bar">>]))),
      ?_test(?assert(not jerk_constraint:validate(Unique, [<<"foo">>, <<"foo">>]))),
      ?_test(?assert(jerk_constraint:validate(Unique, [1, 1.0]))),
      ?_test(?assert(not jerk_constraint:validate(Unique, [1, 2, 1]))),
      ?_test(?assert(not jerk_constraint:validate(Unique, [#{<<"foo">> => [1]},
                                                           #{<<"foo">> => [1]}]))),
      ?_test(?assert(jerk_constraint:validate(Unique, [#{<<"foo">> => [1]},
                                                       #{<<"foo">> => [2]}])))]}.
