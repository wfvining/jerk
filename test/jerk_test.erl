-module(jerk_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    {ok, Sup} = jerk_sup:start_link().

new_jerkterm_test_() ->
    {"A jerk term can be created for schemas that have been loaded",
     {setup, fun start_with_schemas/0, fun stop/1,
      {inparallel,
       [?_test(?assertEqual({<<"foo">>, #{}}, jerk:new(<<"foo">>, []))),
        ?_test(?assertEqual({<<"foo">>, #{<<"bar">> => <<"biz">>}},
                            jerk:new(<<"foo">>, [{<<"bar">>, <<"biz">>}]))),
        ?_test(?assertError(badarg,
                            jerk:new(<<"foo">>, [{<<"bar">>, <<"foo">>}]))),
        ?_test(?assertError(badarg,
                            jerk:new(<<"fiz">>, [])))]}}}.

start_with_schemas() ->
    {ok, Sup} = start(),
    SchemaFoo =
        jiffy:encode(
          #{<<"$id">> => <<"foo">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"bar">> => #{<<"type">> => <<"string">>,
                                 <<"enum">> => [<<"biz">>, <<"baz">>]}}}),
    SchemaBar =
        jiffy:encode(
          #{<<"$id">> => <<"bar">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"a">> => #{<<"$ref">> => <<"#/definitions/TypeA">>},
                  <<"b">> => #{<<"type">> => <<"integer">>},
                  <<"c">> => #{<<"type">> => <<"object">>,
                               <<"properties">> =>
                                   #{<<"d">> =>
                                         #{<<"type">> => <<"integer">>}}}},
            <<"definitions">> =>
                #{<<"TypeA">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> =>
                            #{<<"name">> => #{<<"type">> => <<"string">>},
                              <<"id">> => #{<<"type">> => <<"integer">>,
                                            <<"minimum">> => 1}},
                        <<"required">> => [<<"name">>]}},
            <<"required">> => [<<"a">>],
            <<"additionalProperties">> => false}),
    ok = jerk:add_schema(SchemaFoo),
    ok = jerk:add_schema(SchemaBar),
    Sup.

stop(Sup) ->
    exit(Sup, normal),
    timer:sleep(100).

get_id_test_() ->
    {"Can get the id of the schema that applies to a jerk term.",
     {setup,
      fun start_with_schemas/0, fun stop/1,
      [fun() ->
               Term = jerk:new(<<"foo">>, [{<<"bar">>, <<"baz">>}]),
               ?assertEqual(<<"foo">>, jerk:id(Term))
       end]}}.

attributes_test_() ->
    {setup, fun start_with_schemas/0, fun stop/1,
     {inparallel,
      [{"Can get a list of defined attributes.",
        [?_test(?assertEqual([], jerk:attributes(jerk:new(<<"foo">>, [])))),
         ?_test(
            ?assertEqual(
               [<<"bar">>],
               jerk:attributes(jerk:new(<<"foo">>, [{<<"bar">>, <<"biz">>}])))),
         ?_test(
            ?assertEqual(
               [<<"foo">>],
               jerk:attributes(jerk:new(<<"foo">>, [{<<"foo">>, 1}]))))]},
        {"Can get the value of attributes",
         [?_test(
             ?assertEqual(
                <<"biz">>,
                jerk:get_value(jerk:new(<<"foo">>, [{<<"bar">>, <<"biz">>}]), <<"bar">>))),
          ?_test(
             ?assertEqual(
                1,
                jerk:get_value(jerk:new(<<"foo">>, [{<<"foo">>, 1}]), <<"foo">>)))]},
       {"Getting a non-existent attribute results in a badarg error.",
        [?_test(
            ?assertError(
               {undefined, <<"bar">>},
               jerk:get_value(jerk:new(<<"foo">>, []), <<"bar">>))),
         ?_test(
            ?assertError(
               {undefined, <<"foo">>},
               jerk:get_value(
                 jerk:new(<<"foo">>, [{<<"bar">>, <<"biz">>}]), <<"foo">>)))]},
       {"Set value of an attribute.",
        [?_test(
            ?assertEqual(
               <<"biz">>,
               jerk:get_value(
                 jerk:set_value(jerk:new(<<"foo">>, []), <<"bar">>, <<"biz">>),
                 <<"bar">>))),
         ?_test(
            ?assertError(
               badvalue,
               jerk:set_value(jerk:new(<<"foo">>, []), <<"bar">>, <<"bad">>)))]},
      {"Update the value of an attribute.",
       ?_test(
          ?assertEqual(
             <<"biz">>,
             jerk:get_value(
               jerk:set_value(jerk:new(<<"foo">>, [{<<"bar">>, <<"baz">>}]),
                              <<"bar">>, <<"biz">>),
               <<"bar">>)))}]}}.

nested_term_test_() ->
    {setup, fun start_with_schemas/0, fun stop/1,
     {inparallel,
      [fun create_nested_term/0,
       fun create_invalid_nested_term/0,
       fun set_nested_value/0]}}.

create_nested_term() ->
    Term = jerk:new(
             <<"bar">>,
             [{<<"b">>, 1},
              {<<"a">>, [{<<"name">>, <<"test">>}]},
              {<<"c">>, [{<<"d">>, 2}]}]),
    ?assertEqual(1, jerk:get_value(Term, <<"b">>)),
    ?assertEqual(<<"test">>,
                 jerk:get_value(jerk:get_value(Term, <<"a">>), <<"name">>)).

create_invalid_nested_term() ->
    ?assertError(
       badarg,
       jerk:new(
         <<"bar">>,
         [{<<"b">>, 1},
          {<<"a">>, [{<<"name">>, <<"test">>}, {<<"id">>, 1.2}]},
          {<<"c">>, [{<<"d">>, 2}]}])).

set_nested_value() ->
    Term = jerk:new(
             <<"bar">>,
             [{<<"b">>, 1},
              {<<"a">>, [{<<"name">>, <<"test">>}]},
              {<<"c">>, [{<<"d">>, 2}]}]),
    ?assertEqual(
       jerk:new(<<"bar">>,
                [{<<"b">>, 1},
                 {<<"a">>, [{<<"name">>, <<"test">>}]},
                 {<<"c">>, [{<<"d">>, -1}]}]),
       jerk:set_value(Term, <<"c">>,
                      jerk:set_value(
                        jerk:get_value(Term, <<"c">>), <<"d">>, -1))),
    ?assertEqual(
       jerk:new(<<"bar">>,
                [{<<"b">>, 1},
                 {<<"a">>, [{<<"name">>, <<"test">>}, {<<"id">>, 1}]},
                 {<<"c">>, [{<<"d">>, 2}]}]),
       jerk:set_value(Term, <<"a">>,
                      jerk:set_value(
                        jerk:get_value(Term, <<"a">>), <<"id">>, 1))),
    ?assertError(
       badvalue,
       jerk:set_value(Term, <<"a">>,
                      jerk:set_value(
                        jerk:get_value(Term, <<"a">>), <<"id">>, -1))),
    ?assertError(
       badvalue,
       jerk:set_value(Term, <<"a">>, <<"illegal">>)).
