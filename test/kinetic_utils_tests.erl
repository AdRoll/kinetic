-module(kinetic_utils_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").


test_setup() ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(_Method,
                                    {Url, [{"Connection", "close"}]},
                                    [{timeout, 200000}, {relaxed, true}],
                                    []) ->
                case Url of
                    "/500" ->
                        {ok, {{a, 500, b}, [headers], body}};
                    "/json" ->
                        {ok, {{a, 200, b}, [headers], <<"{\"hello\": \"world\"}">>}};
                    "/error" ->
                        {error, someerror}
                end
    end).

test_teardown(_) ->
    meck:unload(httpc).

kinetic_utils_test_() ->
    {inorder,
        {setup,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?_test(test_endpoint()),
                ?_test(test_json_encoding_decoding()),
                ?_test(test_fetch_and_return_url())
            ]
        }
    }.



%%
%% Tests
%%
test_fetch_and_return_url() ->
    {ok, [{<<"hello">>, <<"world">>}]} = kinetic_utils:fetch_and_return_url("/json"),
    {error, 500} = kinetic_utils:fetch_and_return_url("/500"),
    {error, someerror} = kinetic_utils:fetch_and_return_url("/error"),
    {ok, <<"{\"hello\": \"world\"}">>} = kinetic_utils:fetch_and_return_url("/json", text).

test_json_encoding_decoding() ->
    {error, _} = kinetic_utils:encode({whatever}),
    {error, _} = kinetic_utils:decode(<<"{\"whatever\"}">>),
    {error, _} = kinetic_utils:decode(<<"hello">>),
    [] = kinetic_utils:decode(<<"">>),
    {error, not_a_dict} = kinetic_utils:decode(<<"\"hello\"">>),
    [{<<"hello">>, <<"world">>}] = kinetic_utils:decode(<<"{\"hello\": \"world\"}">>).

test_endpoint() ->
    Service = "kinesis",
    Regions = ["us-east-1", "us-west-1", "us-west-2",
               "eu-west-1", "ap-northeast-1", "ap-southeast-1"],
    lists:foreach(fun(Region) ->
        Url = Service ++ "." ++ Region ++ ".amazonaws.com",
        Url = kinetic_utils:endpoint(Service, Region)
    end, Regions).


