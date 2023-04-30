-module(jerk_loader_test).

-include_lib("eunit/include/eunit.hrl").

load_empty_test() ->
    Schema = <<"{\"$id\": \"foo\", \"type\": \"object\"}">>,
    ?assertEqual([{<<"foo">>, object, {[], [], false}}],
                 jerk_loader:load_json(Schema)).

unconstrained_property_types_test_() ->
    Tests =
        [{#{<<"bar">> => #{<<"type">> => <<"null">>}},
          {<<"bar">>, null, []}},
         {#{<<"bar">> => #{<<"type">> => <<"string">>}},
          {<<"bar">>, string, []}},
         {#{<<"bar">> => #{<<"type">> => <<"integer">>}},
          {<<"bar">>, integer, []}},
         {#{<<"bar">> => #{<<"type">> => <<"number">>}},
          {<<"bar">>, number, []}},
         {#{<<"bar">> => #{<<"type">> => <<"boolean">>}},
          {<<"bar">>, boolean, []}},
         {#{<<"bar">> => #{<<"type">> => <<"array">>}},
          {<<"bar">>, array, []}},
         {#{<<"bar">> => #{<<"type">> => <<"object">>}},
          {<<"bar">>, object, {[], [], false}}}],
    [?_assertEqual(
       [{<<"foo">>, object, {[ExpectedType], [], false}}],
       jerk_loader:load_json(JSON))
     || {JSON, ExpectedType}
            <- [{jiffy:encode(#{<<"$id">> => <<"foo">>,
                                <<"type">> => <<"object">>,
                                <<"properties">> => Properties}),
                 Expected} || {Properties, Expected} <- Tests]].
