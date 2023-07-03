-module(jerk_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    {ok, _Sup} = jerk_sup:start_link().

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
    SchemaArr =
        jiffy:encode(
          #{<<"$id">> => <<"arr">>,
            <<"type">> => <<"object">>,
            <<"definitions">> =>
                #{<<"Element">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"x">> => #{<<"type">> => <<"integer">>}},
                        <<"required">> => [<<"x">>]}},
            <<"properties">> =>
                #{<<"arr">> => #{<<"type">> => <<"array">>,
                                 <<"items">> => #{<<"$ref">> => <<"#/definitions/Element">>},
                                 <<"minItems">> => 1}},
           <<"required">> => [<<"arr">>]}),
    ok = jerk:add_schema(SchemaFoo),
    ok = jerk:add_schema(SchemaBar),
    ok = jerk:add_schema(SchemaArr),
    Sup.

stop(Sup) ->
    exit(Sup, normal),
    timer:sleep(100).

is_primitive_test_() ->
    {"Check if a term is a jerk:primitive/0.",
     {setup, fun start_with_schemas/0, fun stop/1,
      [{inparallel,
        [?_assert(jerk:is_primitive(X)) ||
            X <- [[1, 2], 1, 2.2, true, null, <<"asdf">>]]},
       ?_assert(not jerk:is_primitive(
                      jerk:new(<<"foo">>, [{<<"bar">>, <<"baz">>}])))]}}.

is_object_test_() ->
    {"Check if a term is a jerk:object/0.",
     {setup, fun start_with_schemas/0, fun stop/1,
      {inparallel,
       [?_assert(jerk:is_object(jerk:new(<<"foo">>, [{<<"bar">>, <<"baz">>}]))),
        ?_assert(
           jerk:is_object(
             jerk:get_value(
               jerk:new(
                 <<"bar">>,
                 [{<<"a">>, [{<<"name">>, <<"TestName">>}]}]),
               <<"a">>))),
        [?_assert(not jerk:is_object(X)) ||
            X <- [[1, 2], 1, 2.2, true, null, <<"asdf">>]]]}}}.

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

start_with_file() ->
    Path = filename:join(
             ["/tmp",
              calendar:system_time_to_rfc3339(
                erlang:system_time(millisecond),
                [{unit, millisecond}]) ++ "_jerk_test.json"]),
    ok = file:write_file(
           Path,
           <<"{\"$id\": \"urn:foo\", \"type\": \"object\","
             "\"properties\": {\"a\": {\"type\": \"integer\"},"
             "\"b\": {\"type\": \"object\", \"properties\":"
             "{\"foo\": {\"type\": \"boolean\"}, \"bar\": {"
             "\"type\": \"string\", \"enum\": [\"abc\", \"def\"]}}}}}">>),
    {ok, Sup} = start(),
    {Path, Sup}.

cleanup_with_file({Path, Sup}) ->
    stop(Sup),
    file:delete(Path).

load_schema_test_() ->
    {"Can load a schema from a file.",
     {setup, fun start_with_file/0, fun cleanup_with_file/1,
      fun({Path, _}) ->
              {inorder,
               [?_test(?assertEqual(ok, jerk:load_schema(Path))),
                ?_test(
                   ?assertError(
                      badarg,
                      jerk:new(<<"urn:foo">>, [{<<"a">>, <<"should be an integer">>},
                                               {<<"b">>, []}]))),
                fun() ->
                        T = jerk:new(
                              <<"urn:foo">>,
                              [{<<"a">>, 1},
                               {<<"b">>, [{<<"foo">>, true},
                                          {<<"bar">>, <<"def">>}]}]),
                        ?assertEqual(1, jerk:get_value(T, <<"a">>)),
                        ?assertEqual(
                           <<"def">>,
                           jerk:get_value(jerk:get_value(T, <<"b">>), <<"bar">>))
                end]}
      end}}.

load_no_file_test() ->
    Path = filename:join(code:priv_dir(jerk), "doesnotexist.json"),
    ?assertEqual({error, enoent}, jerk:load_schema(Path)).

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
                 jerk:get_value(jerk:get_value(Term, <<"a">>), <<"name">>)),
    A = jerk:new(<<"bar#/definitions/TypeA">>,
                 [{<<"name">>, <<"test3">>}, {<<"id">>, 3}]),
    C = jerk:new(<<"bar#/properties/c">>, [{<<"d">>, 2}]),
    T2 = jerk:new(<<"bar">>, [{<<"b">>, 1}, {<<"a">>, A}]),
    ?assertEqual(<<"test3">>,
                 jerk:get_value(jerk:get_value(T2, <<"a">>), <<"name">>)),
    ?assertError(badarg,
                 jerk:new(<<"bar">>, [{<<"b">>, 1}, {<<"a">>, C}])).

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

object_in_array_test_() ->
    {"Can construct a term with an array of objects",
     {setup, fun start_with_schemas/0, fun stop/1,
      [fun construct_object_with_array_of_objects/0]}}.

construct_object_with_array_of_objects() ->
    jerk:new(<<"arr">>,
             [{<<"arr">>, [[{<<"x">>, 1}], [{<<"x">>, 2}]]}]).
