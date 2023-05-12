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

bad_type_test() ->
    ?assert(
       not jerk_validator:validate(
             3, string, [jerk_constraint:length(min, 1)])).
