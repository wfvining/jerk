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

item_count_constraint_test_() ->
    MaxItemsConstraint = jerk_constraint:item_count(max, 3),
    MinItemsConstraint = jerk_constraint:item_count(min, 1),
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

item_number_constraint_test_() ->
    NumberConstraints = [jerk_constraint:range(lb, inclusive, 3),
                         jerk_constraint:range(ub, exclusive, 5)],
    Constraint = jerk_constraint:items(
                   number, NumberConstraints),
    {"all elements must satisfy items constraints",
     [?_test(?assertEqual(
                {continue, [{3, {number, NumberConstraints}},
                            {4, {number, NumberConstraints}}]},
                jerk_constraint:validate(Constraint, [3, 4]))),
      ?_test(?assertEqual(
                {continue, []},
                jerk_constraint:validate(Constraint, [])))]}.

primitive_items_test_() ->
    {inparallel,
     [?_assertEqual(
         {continue, [{<<"anything">>, {Type, []}},
                     {1, {Type, []}},
                     {2.0, {Type, []}}]},
         jerk_constraint:validate(
           jerk_constraint:items(Type, []),
           [<<"anything">>, 1, 2.0]))
      || Type <- [string, null, integer, number, boolean,
                  array, ref, object]]}.

object_items_test_() ->
    ObjectDescription =
        {[{<<"a">>, integer,
           [jerk_constraint:range(ub, inclusive, 0)]},
          {<<"b">>, string, [jerk_constraint:length(max, 4),
                             jerk_constraint:length(min, 1)]}],
         [<<"a">>],
         false},
    Constraint =
        jerk_constraint:items(
          object,
          ObjectDescription),
    [?_test(?assertMatch({continue, [_, _]},
                         jerk_constraint:validate(
                           Constraint,
                           [#{<<"a">> => 1, <<"b">> => 2},
                            #{<<"a">> => 0, <<"b">> => <<"foo">>}]))),
     ?_test(?assertEqual({continue, [{1, {object, ObjectDescription}}]},
                         jerk_constraint:validate(
                           Constraint, [1]))),
     ?_test(?assertEqual({continue, [{#{<<"a">> => 0, <<"b">> => <<"foo">>},
                                      {object, ObjectDescription}}]},
                         jerk_constraint:validate(
                           Constraint,
                           [#{<<"a">> => 0, <<"b">> => <<"foo">>}])))].

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

length_constraint_test_() ->
    MinLenConstraint = jerk_constraint:length(min, 2),
    MaxLenConstraint = jerk_constraint:length(max, 4),
    Strings = [{<<""/utf8>>, 0}, {<<"a"/utf8>>, 1}, {<<"ab"/utf8>>, 2},
               {<<"abc"/utf8>>, 3}, {<<"abcd"/utf8>>, 4},
               {<<"abcde"/utf8>>, 5}, {<<"α"/utf8>>, 1},
               {<<"αβ"/utf8>>, 2}, {<<"αβα"/utf8>>, 3},
               {<<"αβαβ"/utf8>>, 4}, {<<"αβαβα"/utf8>>, 5}],
    [{"minimum length constraints",
      {inparallel,
       [?_test(
           ?assertEqual(Len >= 2,
                        jerk_constraint:validate(MinLenConstraint, Str)))
        || {Str, Len} <- Strings]}},
     {"maximum length constraints",
      {inparallel,
       [?_test(
          ?assertEqual(Len =< 4,
                       jerk_constraint:validate(MaxLenConstraint, Str)))
        || {Str, Len} <- Strings]}}].
