-module(jerk_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    {ok, Sup} = jerk_sup:start_link().

add_schema_test_() ->
    SchemaStr =
        jiffy:encode(
          #{<<"$id">> => <<"foo">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"bar">> => #{<<"type">> => <<"string">>,
                                 <<"enum">> => [<<"biz">>, <<"baz">>]}}}),
    %% This is what the schema should look like in the catalog
    %% {<<"foo">>, object,
    %%  {[{<<"bar">>, string,
    %%     [jerk_constraint:enum([<<"biz">>, <<"baz">>])]}],
    %%   [],
    %%   false}},
    {"A schema can be added to the catalog and looked up by its ID.",
     {setup, fun start/0,
      {inorder,
       [?_test(?assertEqual(ok, jerk:add_schema(SchemaStr))),
        {inparallel,
         [?_test(?assertEqual({<<"foo">>, #{}}, jerk:new(<<"foo">>, []))),
          ?_test(?assertEqual({<<"foo">>, #{<<"bar">> => <<"biz">>}},
                              jerk:new(<<"foo">>, [{<<"bar">>, <<"biz">>}]))),
          ?_test(?assertError(badarg,
                              jerk:new(<<"foo">>, [{<<"bar">>, <<"foo">>}]))),
          ?_test(?assertError(badarg,
                              jerk:new(<<"fiz">>, [])))]}]}}}.
