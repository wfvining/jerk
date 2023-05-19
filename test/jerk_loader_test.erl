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
     fun nested_schema/0,
     fun(Definition) ->
             [?_test(
                 ?assertEqual(
                    {<<"foo/definitions/bar">>, integer, []},
                    lists:keyfind(<<"foo/definitions/bar">>, 1, Definition))),
              ?_test(
                 ?assertMatch(
                    {<<"foo/definitions/baz">>, object, {[_], [], false}},
                    lists:keyfind(<<"foo/definitions/baz">>, 1, Definition))),
              ?_test(
                 begin
                     {_, _, {BazProperties, _, _}} =
                         lists:keyfind(<<"foo/definitions/baz">>, 1, Definition),
                     ?assertEqual([{<<"a">>, boolean, []}], BazProperties)
                 end),
              ?_test(
                 ?assertEqual(
                    {<<"foo">>, object, {[], [], false}},
                    lists:keyfind(<<"foo">>, 1, Definition)))]
     end}.

nested_schema() ->
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
    jerk_loader:load_json(Schema).

schema_property_reference() ->
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
                                  #{<<"type">> => <<"boolean">>}}}},
            <<"properties">> =>
                #{<<"p1">> => #{<<"$ref">> => <<"#/definitions/bar">>},
                  <<"p2">> => #{<<"$ref">> => <<"#/definitions/baz">>}}}),
    jerk_loader:load_json(Schema).

definition_reference_test_() ->
    {setup, fun schema_property_reference/0,
     {with,
      [fun (Definition) ->
               {<<"foo">>, object, {Properties, [], false}} =
                   lists:keyfind(<<"foo">>, 1, Definition),
               ?assertEqual({<<"p1">>, ref, <<"#/definitions/bar">>},
                            lists:keyfind(<<"p1">>, 1, Properties)),
               ?assertEqual({<<"p2">>, ref, <<"#/definitions/baz">>},
                            lists:keyfind(<<"p2">>, 1, Properties))
       end]}}.

array_constraint_test_() ->
    [{"array item constraints",
      [fun item_value_constraint/0, fun item_count_constraint/0,
       fun item_object_constraint/0]},
     {"array unique constraints",
      array_unique_constraint()}].

item_value_constraint() ->
    Schema = jiffy:encode(
               #{<<"$id">> => <<"foo">>,
                 <<"type">> => <<"array">>,
                 <<"items">> => #{<<"type">> => <<"integer">>,
                                  <<"minimum">> => 0,
                                  <<"exclusiveMaximum">> => 3}}),
    ?assertMatch(
       [{<<"foo">>, array,
         [{allowed, {integer, [_, _]}}]}],
       jerk_loader:load_json(Schema)),
    [{<<"foo">>, array,
      [{allowed, {integer, [_, _] = Constraints}}]}] =
        jerk_loader:load_json(Schema),
    ?assert(lists:member({lb, {inclusive, 0}}, Constraints)),
    ?assert(lists:member({ub, {exclusive, 3}}, Constraints)).

item_object_constraint() ->
    Schema = jiffy:encode(
               #{<<"$id">> => <<"foo">>,
                 <<"type">> => <<"array">>,
                 <<"items">> =>
                     #{<<"type">> => <<"object">>,
                       <<"properties">> =>
                           #{<<"a">> => #{<<"type">> => <<"integer">>,
                                          <<"minimum">> => 0},
                             <<"b">> => #{<<"type">> => <<"string">>}},
                       <<"required">> => [<<"a">>]}}),
    [{<<"foo">>, array,
      [{allowed, {object, ObjectDescription}}]}] =
        jerk_loader:load_json(Schema),
    ?assertMatch({[_, _], [<<"a">>], false}, ObjectDescription),
    Properties = element(1, ObjectDescription),
    ?assertEqual({<<"a">>, integer, [{lb, {inclusive, 0}}]},
                 lists:keyfind(<<"a">>, 1, Properties)),
    ?assertEqual({<<"b">>, string, []},
                 lists:keyfind(<<"b">>, 1, Properties)).

item_count_constraint() ->
    Schema = jiffy:encode(
               #{<<"$id">> => <<"foo">>,
                 <<"type">> => <<"array">>,
                 <<"minItems">> => 2,
                 <<"maxItems">> => 3}),
    [{<<"foo">>, array, Constraints}] = jerk_loader:load_json(Schema),
    ?assert(lists:member({items, {min, 2}}, Constraints)),
    ?assert(lists:member({items, {max, 3}}, Constraints)),
    ?assertEqual([], Constraints -- [{items, {min, 2}}, {items, {max, 3}}]).

array_unique_constraint() ->
    [?_test(
        begin
            Schema = jiffy:encode(
                       #{<<"$id">> => <<"foo">>,
                         <<"type">> => <<"array">>,
                         <<"uniqueItems">> => true}),
            ?assertEqual(
               [{<<"foo">>, array, [{unique, true}]}],
               jerk_loader:load_json(Schema))
        end),
     ?_test(
        begin
            Schema = jiffy:encode(
                       #{<<"$id">>  => <<"foo">>,
                         <<"type">> => <<"array">>,
                         <<"uniqueItems">> => false}),
            ?assertEqual(
               [{<<"foo">>, array, []}],
               jerk_loader:load_json(Schema))
        end)].

unconstrained_type_test() ->
    Schema = jiffy:encode(
               #{<<"$id">> => <<"foo">>,
                 <<"type">> => <<"object">>,
                 <<"properties">> =>
                     #{<<"foo">> => #{<<"irrelevant">> => <<"stuff">>}}}),
    ?assertEqual([{<<"foo">>, object,
                   {[{<<"foo">>, any, nil}], [], false}}],
                 jerk_loader:load_json(Schema)).
