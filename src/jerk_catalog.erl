-module(jerk_catalog).

-behaviour(gen_server).

-export([start_link/0, add_schema/1, get_schema/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_schema(Schema) ->
    gen_server:call(?SERVER, {add_schema, Schema}).

get_schema(SchemaID) ->
    case ets:lookup(?SERVER, SchemaID) of
        [] ->
            get_subschema(SchemaID);
        [Schema] ->
            Schema
    end.

get_subschema(SchemaURI) ->
    case string:split(SchemaURI, <<"#/">>) of
        [BaseURI, Path] ->
            Schema = get_schema(BaseURI),
            get_subschema(Path, Schema);
        [SchemaURI] ->
            error(badarg)
    end.

get_subschema(<<"properties/", Id/binary>>,
              {_, object, {Properties, _, _}}) ->
    case lists:keyfind(Id, 1, Properties) of
        false ->
            error(badarg);
        Description ->
            Description
    end.

init([]) ->
    Tref = ets:new(?SERVER, [named_table, set, {read_concurrency, true}]),
    {ok, Tref}.

handle_call({add_schema, Schema}, _From, Table) ->
    ets:insert(Table, Schema),
    {reply, ok, Table};
handle_call(Call, From, State) ->
    logger:warning("unnexpected call ~p from ~p", [Call, From]),
    {noreply, State}.

handle_cast(Cast, Table) ->
    logger:warning("unnexpected cast ~p", [Cast]),
    {noreply, Table}.
