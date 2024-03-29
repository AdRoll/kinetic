-module(kinetic_sup_tests).

-include("kinetic.hrl").

-include_lib("eunit/include/eunit.hrl").

-hank([{unnecessary_function_arguments, [{test_teardown, 1, 1}]}]).

test_setup() ->
    meck:new(erliam, [passthrough]),
    meck:expect(erliam, invalidate, 0, ok),
    meck:expect(erliam, credentials, 0, fake_creds),
    meck:new(imds, [passthrough]),
    meck:expect(imds, zone, 0, {ok, "us-east-1b"}).

test_teardown(_) ->
    meck:unload(imds),
    meck:unload(erliam).

kinetic_sup_test_() ->
    {inorder, {foreach, fun test_setup/0, fun test_teardown/1, [?_test(test_supervisor())]}}.

test_supervisor() ->
    process_flag(trap_exit, true),
    {ok, Pid} =
        kinetic_sup:start_link([{aws_access_key_id, "whatever"},
                                {aws_secret_access_key, "secret"}]),
    {ok,
     #kinetic_arguments{aws_credentials = fake_creds,
                        region = "us-east-1",
                        lhttpc_opts = []}} =
        kinetic_config:get_args(),

    kinetic_sup:stop(Pid),
    {error, _} = kinetic_config:get_args(),
    process_flag(trap_exit, false).
