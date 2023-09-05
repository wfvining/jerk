-module(jerk_catalog).

-behaviour(gen_server).

-export([start_link/0, add_schema/1, get_schema/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_schema(Schema) ->
    gen_server:call(?SERVER, {add_schema, Schema}).

%% @doc Return the schema associated with `SchemaID'. `SchemaID' may
%% refer to a sub-schema or the schema for a specific property
%% (e.g. "urn:foo#/properties/x"). If no schema exists in the catalog
%% associated with the schema ID then the call fails with reason
%% `badarg'.
get_schema(SchemaID) ->
    case ets:lookup(?SERVER, SchemaID) of
        [] ->
            get_subschema(SchemaID);
        [Schema] ->
            Schema
    end.

get_subschema(SchemaURI) ->
    case string:split(SchemaURI, <<"#/">>) of
        [BaseURI, <<"definitions/", Rest/binary>>] ->
            [Id|Path] = binary:split(Rest, <<"/">>, [global]),
            Schema = get_schema(<<BaseURI/binary, "#/definitions/", Id/binary>>),
            get_subschema(Path, Schema);
        [BaseURI, Path] ->
            Schema = get_schema(BaseURI),
            get_subschema(binary:split(Path, <<"/">>, [global]), Schema);
        [SchemaURI] ->
            error(badarg)
    end.

get_subschema([], Schema) ->
    Schema;
get_subschema([<<"properties">>, Id | Rest],
              {_, object, {Properties, _, _}}) ->
    case lists:keyfind(Id, 1, Properties) of
        false ->
            error(badarg);
        Description ->
            get_subschema(Rest, Description)
    end;
get_subschema([<<"$array-item">> | Rest], {_, array, Constraints}) ->
    case lists:keyfind(allowed, 1, Constraints) of
        {allowed, {Type, Description}} ->
            {<<"$array-item">>, Type, Description};
        _ ->
            error(badarg)
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
