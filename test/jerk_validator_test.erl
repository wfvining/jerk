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
