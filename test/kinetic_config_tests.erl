-module(kinetic_config_tests).

-include("kinetic.hrl").

-include_lib("eunit/include/eunit.hrl").

-hank([{unnecessary_function_arguments, [{test_teardown, 1, 1}]}]).

test_setup() ->
    meck:new(erliam, [passthrough]),
    meck:expect(erliam, invalidate, 0, ok),
    meck:expect(erliam, credentials, 0, fake_creds),
    meck:new(imds, [passthrough]),
    meck:expect(imds, zone, 0, {ok, "us-east-1b"}),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer,
                apply_interval,
                fun(Interval, M, F, [Opts]) ->
                   case proplists:get_value(should_err, Opts) of
                       true ->
                           {error, broken};
                       _ ->
                           meck:passthrough([Interval, M, F, [Opts]])
                   end
                end).

test_teardown(_) ->
    meck:unload(timer),
    meck:unload(imds),
    meck:unload(erliam).

kinetic_config_test_() ->
    {inorder,
     {foreach,
      fun test_setup/0,
      fun test_teardown/1,
      [?_test(test_passed_metadata()), ?_test(test_config_env())]}}.

merge_args_test_() ->
    [{"overriding the region should affect the region, host, and url",
      ?_test(begin
                 Args1 =
                     #kinetic_arguments{region = "region1",
                                        host = Host1 = "host1",
                                        url = Url1 = "url1"},
                 #kinetic_arguments{region = Region2,
                                    host = Host2,
                                    url = Url2} =
                     kinetic_config:merge_args(Args1, [{region, "us-east-1"}]),
                 ?assertEqual("us-east-1", Region2),
                 ?assertNotEqual(Host1, Host2),
                 ?assertNotEqual(Url1, Url2),
                 ok
             end)},
     {"it should be possible to override the timeout",
      ?_test(begin
                 Args = kinetic_config:merge_args(#kinetic_arguments{timeout = 1}, [{timeout, 2}]),
                 ?assertEqual(2, Args#kinetic_arguments.timeout),
                 ok
             end)}].

test_config_env() ->
    application:set_env(kinetic, whatever, value),
    value = kinetic_config:g(whatever),
    undefined = kinetic_config:g(something).

test_passed_metadata() ->
    {ok, _Pid} =
        kinetic_config:start_link([{aws_access_key_id, "whatever"},
                                   {aws_secret_access_key, "secret"}]),
    ?assert(ets:info(?KINETIC_STREAM) =/= undefined),
    {ok,
     #kinetic_arguments{aws_credentials = fake_creds,
                        region = "us-east-1",
                        lhttpc_opts = []}} =
        kinetic_config:get_args(),
    kinetic_config:update_data([{aws_access_key_id, "whatever"},
                                {aws_secret_access_key, "secret"}]),
    {ok,
     #kinetic_arguments{aws_credentials = fake_creds,
                        region = "us-east-1",
                        lhttpc_opts = []}} =
        kinetic_config:get_args(),
    kinetic_config:stop(),
    {error, _} = kinetic_config:get_args(),
    undefined = ets:info(?KINETIC_STREAM).
