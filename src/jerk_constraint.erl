-module(jerk_constraint).

-export([contains/1,
         enum/1,
         items/2,
         length/2,
         multiple/1,
         range/3,
         unique/0]).

-export([validate/2]).

-export_type([sense/0,
              bound/0,
              contains/1,
              enum/1,
              items/0,
              length/0,
              multipleof/0,
              range/0,
              unique/0]).

-type sense() :: min | max.
-type bound() :: lb | ub.

-type contains(D) :: {contains, D}.
-type enum(Values) :: {enum, Values}.
-type items() :: {items, {sense(), non_neg_integer()}}.
-type length() :: {length, {sense(), non_neg_integer()}}.
-type multipleof() :: {multipleof, number()}.
-type range() :: {bound(), {inclusive | exclusive, number()}}.
-type unique() :: {unique, boolean()}.

-type constraint() :: contains(_)
                    | enum(_)
                    | items()
                    | length()
                    | multipleof()
                    | range()
                    | unique().

-spec contains(Definition) -> contains(Definition).
contains(Definition) ->
    {contains, Definition}.

-spec enum(Values :: [ValueType]) -> enum(ValueType).
enum(Values) when is_list(Values) ->
    {enum, Values};
enum(_) ->
    error(badarg).

-spec items(Sense :: sense(), Value :: non_neg_integer()) -> items().
items(Sense, Value)
  when (is_integer(Value) andalso Value >= 0),
       ((Sense =:= min) or (Sense =:= max)) ->
    {items, {Sense, Value}};
items(_, _) ->
    error(badarg).

-spec length(Sense :: sense(), Value :: non_neg_integer()) -> length().
length(Sense, Value)
  when (is_integer(Value) andalso Value >= 0),
       ((Sense =:= min) or (Sense =:= max)) ->
    {length, {Sense, Value}};
length(_, _) ->
    error(badarg).

-spec multiple(Divisor :: number()) -> multipleof().
multiple(Divisor) when is_number(Divisor) ->
    {multipleof, Divisor};
multiple(_) ->
    error(badarg).

-spec range(UbLb :: ub | lb,
            Exclusive :: inclusive | exclusive,
            N :: number()) -> range().
range(UbLb, Exclusive, N)
  when is_number(N),
       ((UbLb =:= ub) or (UbLb =:= lb)),
       ((Exclusive =:= exclusive) or (Exclusive =:= inclusive)) ->
    {UbLb, {Exclusive, N}};
range(_, _, _) ->
    error(badarg).

-spec unique() -> unique().
unique() ->
    {unique, true}.

-spec validate(Constraint :: constraint(), Value :: any()) -> boolean().
validate({ub, {inclusive, N}}, Value) ->
    Value =< N;
validate({ub, {exclusive, N}}, Value) ->
    Value < N;
validate({lb, {inclusive, N}}, Value) ->
    Value >= N;
validate({lb, {exclusive, N}}, Value) ->
    Value > N;

validate({multipleof, Zero}, Value) when Zero == 0; Zero == 0.0 ->
    Value == 0;
validate({multipleof, N}, Value) when is_integer(N), is_integer(Value) ->
    0 =:= Value rem N;
validate({multipleof, N}, Value) ->
    math:fmod(Value, N) =:= 0.0;

validate({enum, Values}, Value) ->
    lists:member(Value, Values);

validate({items, {min, N}}, L) ->
    length(L) >= N;
validate({items, {max, N}}, L) ->
    length(L) =< N;

validate({unique, true}, L) ->
    length(lists:uniq(L)) =:= length(L).
