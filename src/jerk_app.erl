%%% @doc Jerk application.
%%% @end

-module(jerk_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    jerk_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
