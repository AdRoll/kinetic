-module(kinetic_utils_tests).

-include_lib("eunit/include/eunit.hrl").

kinetic_utils_test_() ->
    [?_test(test_endpoint()), ?_test(test_json_encoding_decoding())].

%%
%% Tests
%%

test_json_encoding_decoding() ->
    {error, _} = kinetic_utils:encode({whatever}),
    {error, _} = kinetic_utils:decode(<<"{\"whatever\"}">>),
    {error, _} = kinetic_utils:decode(<<"hello">>),
    [] = kinetic_utils:decode(<<"">>),
    {error, not_a_dict} = kinetic_utils:decode(<<"\"hello\"">>),
    [{<<"hello">>, <<"world">>}] = kinetic_utils:decode(<<"{\"hello\": \"world\"}">>).

test_endpoint() ->
    Service = "kinesis",
    Regions =
        ["us-east-1", "us-west-1", "us-west-2", "eu-west-1", "ap-northeast-1", "ap-southeast-1"],
    lists:foreach(fun(Region) ->
                     Url = Service ++ "." ++ Region ++ ".amazonaws.com",
                     Url = kinetic_utils:endpoint(Service, Region)
                  end,
                  Regions).
