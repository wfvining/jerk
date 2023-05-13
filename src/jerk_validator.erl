%%% @doc Validate objects or terms against schemas.
%%%
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright 2023 Will Vining
-module(jerk_validator).

-export([validate/3]).

%% @doc Check if `Object' is valid against `Schema'.
%%
%% @param Object A jerk object or primitive.
%% @param
-spec validate(Object :: jerk:object() | jerk:primterm(),
               Type :: jerk:type(),
               Constraints :: [jerk_constraint:constraint()]) ->
          boolean().
validate(Object, object, ObjectDescription) when is_map(Object)->
    check_required(Object, ObjectDescription)
        andalso check_params(Object, ObjectDescription);
validate(Object, object, _) when not is_map(Object) ->
    false;
validate(Object, Type, Constraints) ->
    check_type(Object, Type) andalso check_constraints(Object, Constraints).

check_type(X, integer) -> is_integer(X);
check_type(X, number) -> is_number(X);
check_type(X, boolean) -> is_boolean(X);
check_type(X, string) -> is_binary(X);
check_type(X, array) -> is_list(X);
check_type(X, null) -> X =:= null.

check_constraints(X, Constraints) ->
    lists:all(fun(C) -> jerk_constraint:validate(C, X) end, Constraints).

check_required(Object, {_, Required, _}) ->
    lists:all(fun(Param) -> maps:is_key(Param, Object) end,
              Required).

check_params(Object, {Params, _, Frozen}) ->
    maps:fold(
      fun(Param, Value, Acc) ->
              Acc andalso validate_param(Param, Value, Params, Frozen)
      end,
      true,
      Object).

validate_param(Param, Value, Params, Frozen) ->
    KnownParam = lists:keymember(Param, 1, Params),
    ParamDescription = lists:keyfind(Param, 1, Params),
    (not (KnownParam or Frozen))
        orelse validate(Value,
                        element(2, ParamDescription),
                        element(3, ParamDescription)).
