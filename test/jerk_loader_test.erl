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

number_constraint_test_() ->
    Constraints = [#{<<"type">> => <<"number">>,
                     <<"minimum">> => 5,
                     <<"maximum">> => 10.2},
                   #{<<"type">> => <<"number">>,
                     <<"exclusiveMinimum">> => -1.2,
                     <<"exclusiveMaximum">> => 2}],
    Expected = [{<<"bar">>, number, [{lb, {inclusive, 5}},
                                     {ub, {inclusive, 10.2}}]},
                {<<"bar">>, number, [{lb, {exclusive, -1.2}},
                                     {ub, {exclusive, 2}}]}],
    Schemas = [jiffy:encode(
                 #{<<"$id">> => <<"foo">>,
                   <<"type">> => <<"object">>,
                   <<"properties">> => #{<<"bar">> => Constraint}})
               || Constraint <- Constraints],
    Properties =
        [Properties || [{_, object, {Properties, _, _}}]
                           <- lists:map(fun jerk_loader:load_json/1, Schemas)],
    LoadedConstraints =
        [lists:sort(LoadedConstraints) || [{_, _, LoadedConstraints}] <- Properties],
    ExpectedConstraints =
        [lists:sort(ExpectedConstraints) || {_, _, ExpectedConstraints} <- Expected],
    [[?_assertMatch([{<<"bar">>, number, _}], Property) || Property <- Properties],
     [?_assertEqual(Constraint, Result)
      || {Constraint, Result} <- lists:zip(ExpectedConstraints, LoadedConstraints)]].

enum_constraint_test() ->
    ExpectedEnumValues = [<<"Foo">>, <<"Bar">>],
    Schema = jiffy:encode(
                #{<<"$id">> => <<"foo">>,
                  <<"type">> => <<"object">>,
                  <<"properties">> =>
                      #{<<"bar">> =>
                            #{<<"type">> => <<"string">>,
                              <<"enum">> => ExpectedEnumValues}}}),
    [{<<"foo">>, object,
      {[{<<"bar">>, string, [{enum, EnumValues}]}], [], false}}] =
        jerk_loader:load_json(Schema),
    ?assertEqual(lists:sort(ExpectedEnumValues), lists:sort(EnumValues)).

definitions_test_() ->
    {setup,
     fun () ->
             Schema =
                 jiffy:encode(
                   #{<<"$id">> => <<"foo">>,
                     <<"type">> => <<"object">>,
                     <<"definitions">> =>
                         #{<<"bar">> => #{<<"type">> => <<"integer">>},
                           <<"baz">> =>
                               #{<<"type">> => <<"object">>,
                                 <<"properties">> =>
                                     #{<<"a">> =>
                                           #{<<"type">> => <<"boolean">>}}}}}),
             ?debugVal(jerk_loader:load_json(Schema))
     end,
     {with,
      [fun (Definition) ->
               ?assertEqual(
                  {<<"foo/definitions/bar">>, integer, []},
                  lists:keyfind(<<"foo/definitions/bar">>, 1, Definition))
       end,
       fun (Definition) ->
               ?assertMatch(
                  {<<"foo/definitions/baz">>, object, {[_], [], false}},
                  lists:keyfind(<<"foo/definitions/baz">>, 1, Definition))
       end,
       fun (Definition) ->
               {_, _, {BazProperties, _, _}} =
                   lists:keyfind(<<"foo/definitions/baz">>, 1, Definition),
               ?assertEqual([{<<"a">>, boolean, []}], BazProperties)
       end,
       fun (Definition) ->
               ?assertEqual(
                  {<<"foo">>, object, {[], [], false}},
                  lists:keyfind(<<"foo">>, 1, Definition))
       end]}}.
