-module(jerk_constraint).

-export([contains/1,
         enum/1,
         item_count/2,
         items/2,
         length/2,
         multiple/1,
         range/3,
         unique/0]).

-export([validate/2]).

-export_type([sense/0,
              bound/0,
              type/0,
              contains/1,
              enum/1,
              item_count/0,
              items/0,
              length/0,
              multipleof/0,
              range/0,
              unique/0]).

-type type() :: object
              | integer
              | number
              | string
              | array
              | boolean
              | null
              | ref.

-type continue() :: {continue, [{any(), {type(), constraint()}}]}.

-type sense() :: min | max.
-type bound() :: lb | ub.

-type contains(D) :: {contains, D}.
-type items() :: {allowed, {type(), [constraint()]}}.
-type item_count() :: {items, {sense(), non_neg_integer()}}.
-type enum(Values) :: {enum, Values}.
-type length() :: {length, {sense(), non_neg_integer()}}.
-type multipleof() :: {multipleof, number()}.
-type range() :: {bound(), {inclusive | exclusive, number()}}.
-type unique() :: {unique, boolean()}.

-type constraint() :: contains(_)
                    | enum(_)
                    | items()
                    | item_count()
                    | length()
                    | multipleof()
                    | range()
                    | unique().

-type object() :: {[{binary(), type(), [constraint()]}],
                   [binary()],
                   boolean()}.

-if(?OTP_RELEASE >= 25).
-define(uniq(L), lists:uniq(L)).
-else.
-define(uniq(L), uniq(L)).
uniq(L) ->
    uniq(L, []).
uniq([], Result) ->
    Result;
uniq([H|T], Result) ->
    case lists:member(H, Result) of
        true ->
            uniq(T, Result);
        false ->
            uniq(T, [H|Result])
    end.
-endif.

-spec contains(Definition) -> contains(Definition).
contains(Definition) ->
    {contains, Definition}.

-spec enum(Values :: [ValueType]) -> enum(ValueType).
enum(Values) when is_list(Values) ->
    {enum, Values};
enum(_) ->
    error(badarg).

-spec items(Type :: type(), Constraints :: [constraint()] | object()) -> items().
items(Type, Constraints) ->
    {allowed, {Type, Constraints}}.

-spec item_count(Sense :: sense(), Value :: non_neg_integer()) -> item_count().
item_count(Sense, Value)
  when (is_integer(Value) andalso Value >= 0),
       ((Sense =:= min) or (Sense =:= max)) ->
    {items, {Sense, Value}};
item_count(_, _) ->
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

-spec validate(Constraint :: constraint(), Value :: any()) ->
          boolean() | continue().
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
    length(?uniq(L)) =:= length(L);

validate({length, {min, N}}, Str) ->
    string:length(Str) >= N;
validate({length, {max, N}}, Str) ->
    string:length(Str) =< N;

validate({allowed, {Type, Constraints}}, L) when is_list(L) ->
    {continue, [{Value, {Type, Constraints}} || Value <- L]}.
