-module(kinetic_sup_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    meck:new(erliam, [passthrough]),
    meck:expect(erliam, invalidate, 0, ok),
    meck:new(imds, [passthrough]),
    meck:expect(imds, zone, 0, {ok, "us-east-1b"}).

test_teardown(_) ->
    meck:unload(imds),
    meck:unload(erliam).

kinetic_sup_test_() ->
    {inorder,
        {foreach,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?_test(test_supervisor())
            ]
        }
    }.


test_supervisor() ->
    process_flag(trap_exit, true),
    {ok, Pid} = kinetic_sup:start_link([{aws_access_key_id, "whatever"},
                                        {aws_secret_access_key, "secret"}]),
    {ok, #kinetic_arguments{
        region="us-east-1",
        lhttpc_opts=[]}} = kinetic_config:get_args(),

    kinetic_sup:stop(Pid),
    {error, _} = kinetic_config:get_args(),
    process_flag(trap_exit, false).
