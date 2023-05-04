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
