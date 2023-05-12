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
