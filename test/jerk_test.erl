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
                                 <<"enum">> => [<<"biz">>, <<"baz">>]},
                  <<"flag">> => #{<<"type">> => <<"boolean">>}}}),
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

boolean_test_() ->
    {"Can create a term with a boolean property",
     {setup, fun start_with_schemas/0, fun stop/1,
      [?_test(
          ?assert(
             jerk:get_value(jerk:new(<<"foo">>, [{<<"flag">>, true}]),
                            <<"flag">>)))]}}.

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

init_array_schemas() ->
    {ok, Sup} = start(),
    SchemaArrObj =
        jiffy:encode(
          #{<<"$id">> => <<"arrObj">>,
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
    SchemaArrNestedRef =
        jiffy:encode(
          #{<<"$id">> => <<"arrNestedRef">>,
            <<"type">> => <<"object">>,
            <<"definitions">> =>
                #{<<"Element">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#/definitions/X">>}},
                        <<"required">> => [<<"x">>]},
                  <<"X">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"y">> => #{<<"type">> => <<"integer">>},
                                              <<"z">> => #{<<"type">> => <<"array">>,
                                                           <<"items">> => #{<<"$ref">> => <<"#/definitions/Z">>}}}},
                  <<"Z">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"p">> => #{<<"type">> => <<"integer">>}}}},
            <<"properties">> =>
                #{<<"arr">> => #{<<"type">> => <<"array">>,
                                 <<"items">> => #{<<"$ref">> => <<"#/definitions/Element">>},
                                 <<"minItems">> => 1}},
           <<"required">> => [<<"arr">>]}),
    SchemaArrInt =
        jiffy:encode(
          #{<<"$id">> => <<"arrInt">>,
            <<"type">> => <<"object">>,
            <<"definitions">> =>
                #{<<"Element">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"x">> => #{<<"type">> => <<"integer">>}},
                        <<"required">> => [<<"x">>]}},
            <<"properties">> =>
                #{<<"arr">> => #{<<"type">> => <<"array">>,
                                 <<"items">> => #{<<"type">> => <<"integer">>,
                                                  <<"maximum">> => 10},
                                 <<"maxItems">> => 2}},
           <<"required">> => [<<"arr">>]}),
    SchemaArrAnon =
        jiffy:encode(
          #{<<"$id">> => <<"arrAnon">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"arr">> => #{<<"type">> => <<"array">>,
                                 <<"items">> =>
                                     #{<<"type">> => <<"object">>,
                                       <<"properties">> => #{<<"x">> => #{<<"type">> => <<"integer">>}},
                                       <<"required">> => [<<"x">>]}},
                                 <<"minItems">> => 1},
           <<"required">> => [<<"arr">>]}),
    SchemaArrUnconstrained =
        jiffy:encode(
          #{<<"$id">> => <<"arrUnconstrained">>,
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{<<"arr">> => #{<<"type">> => <<"array">>}},
           <<"required">> => [<<"arr">>]}),
    ok = jerk:add_schema(SchemaArrObj),
    ok = jerk:add_schema(SchemaArrInt),
    ok = jerk:add_schema(SchemaArrAnon),
    ok = jerk:add_schema(SchemaArrUnconstrained),
    ok = jerk:add_schema(SchemaArrNestedRef),
    Sup.

array_test_() ->
    {setup, fun init_array_schemas/0, fun stop/1,
     {inparallel,
      [{"construct a term with an empty array",
        fun () ->
                T = jerk:new(<<"arrInt">>, [{<<"arr">>, []}]),
                ?assertEqual([], jerk:get_value(T, <<"arr">>))
        end},
       {"access a term from an array element defined be nested references",
        [fun access_array_elem_nested_ref/0,
         fun access_nested_array_element/0]},
       {"construct an array with an illegal number of items",
        [?_test(?assertError(badarg, jerk:new(<<"arrInt">>, [{<<"arr">>, [1, 2, 3]}]))),
         ?_test(?assertError(badarg, jerk:new(<<"arrObj">>, [{<<"arr">>, []}])))]},
       {"can't construct a term with an array containing "
        "a value that violates a constraint",
        [?_test(?assertError(badarg, jerk:new(<<"arrInt">>, [{<<"arr">>, [11]}]))),
         ?_test(?assertError(badarg, jerk:new(<<"arrInt">>, [{<<"arr">>, [10, 11]}])))]},
       {"update an array",
        [fun update_array/0,
         fun update_array_too_short/0,
         fun update_array_too_long/0,
         fun update_array_illegal_value/0]},
       {"can construct a term with an array of objects where the schema "
        "uses an anonymous object schema for array items",
        construct_object_with_array_of_objects(<<"arrAnon">>)},
       {"can construct a term with an array of objects",
        construct_object_with_array_of_objects(<<"arrObj">>)}]}}.

update_array() ->
    T = jerk:new(<<"arrObj">>, [{<<"arr">>, [[{<<"x">>, 1}]]}]),
    [X] = jerk:get_value(T, <<"arr">>),
    X1 = jerk:set_value(X, <<"x">>, 2),
    T1 = jerk:set_value(T, <<"arr">>, [X1]),
    ?assertEqual([X1], jerk:get_value(T1, <<"arr">>)).

access_array_elem_nested_ref() ->
    T = jerk:new(<<"arrNestedRef">>, [{<<"arr">>, [[{<<"x">>, [{<<"y">>, 1}]}]]}]),
    [Elem] = jerk:get_value(T, <<"arr">>),
    X = jerk:get_value(Elem, <<"x">>),
    ?assert(jerk:is_object(X)),
    ?assertEqual(1, jerk:get_value(X, <<"y">>)).

access_nested_array_element() ->
    T = jerk:new(<<"arrNestedRef">>,
                 [{<<"arr">>, [[{<<"x">>, [{<<"y">>, 1},
                                           {<<"z">>, [[{<<"p">>, 1}], [{<<"p">>, 2}]]}]}]]}]),
    [Elem] = jerk:get_value(T, <<"arr">>),
    X = jerk:get_value(Elem, <<"x">>),
    [Z1, Z2] = jerk:get_value(X, <<"z">>), %% XXX The schema id becomes invalid here
    ?assert(jerk:is_object(Z1)),
    ?assertEqual(1, jerk:get_value(Z1, <<"p">>)),
    ?assertEqual(2, jerk:get_value(Z2, <<"p">>)).

update_array_too_short() ->
    T = jerk:new(<<"arrObj">>, [{<<"arr">>, [[{<<"x">>, 1}]]}]),
    ?assertError(badvalue, jerk:set_value(T, <<"arr">>, [])).

update_array_too_long() ->
    T = jerk:new(<<"arrInt">>, [{<<"arr">>, [10]}]),
    ?assertError(badvalue, jerk:set_value(T, <<"arr">>, [1, 2, 3])).

update_array_illegal_value() ->
    T = jerk:new(<<"arrInt">>, [{<<"arr">>, []}]),
    ?assertError(badvalue, jerk:set_value(T, <<"arr">>, [11])).

construct_object_with_array_of_objects(Schema) ->
    {"array of objects with schema " ++ binary_to_list(Schema),
     fun () ->
             T = jerk:new(Schema,
                          [{<<"arr">>, [[{<<"x">>, 1}], [{<<"x">>, 2}]]}]),
             [A1, A2] = jerk:get_value(T, <<"arr">>),
             ?assertEqual(1, jerk:get_value(A1, <<"x">>)),
             ?assertEqual(2, jerk:get_value(A2, <<"x">>)),
             A1New = jerk:set_value(A1, <<"x">>, 3),
             ?assertEqual(3, jerk:get_value(A1New, <<"x">>)),
             ?assertError(badvalue, jerk:set_value(A1, <<"x">>, <<"not an integer">>))
     end}.

nested_ref_test_() ->
    {"operate on objects that include multiple levels of '$ref' references",
     {setup, fun init_nested_ref_schemas/0, fun stop/1,
      [fun access_nested/0]}}.

init_nested_ref_schemas() ->
    {ok, Sup} = start(),
    Nested =
        jiffy:encode(
          #{<<"$id">> => <<"nested">>,
            <<"type">> => <<"object">>,
            <<"definitions">> =>
                #{<<"Element">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"x">> => #{<<"$ref">> => <<"#/definitions/X">>}},
                        <<"required">> => [<<"x">>]},
                  <<"X">> =>
                      #{<<"type">> => <<"object">>,
                        <<"properties">> => #{<<"y">> => #{<<"type">> => <<"integer">>}}}},
            <<"properties">> =>
                #{<<"elem">> => #{<<"$ref">> => <<"#/definitions/Element">>}},
            <<"required">> => [<<"elem">>]}),
    jerk:add_schema(Nested),
    Sup.

access_nested() ->
    T = jerk:new(<<"nested">>, [{<<"elem">>, [{<<"x">>, [{<<"y">>, 1}]}]}]),
    Elem = jerk:get_value(T, <<"elem">>),
    X = jerk:get_value(Elem, <<"x">>),
    ?assert(jerk:is_object(X)),
    ?assertEqual(1, jerk:get_value(X, <<"y">>)).
