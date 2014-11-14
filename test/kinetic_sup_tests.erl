-module(kinetic_sup_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    meck:new(kinetic_iam),
    meck:expect(kinetic_iam, get_aws_keys,
        fun("error" ++ _Rest, _) ->
                {error, something};
           ("no_expire" ++ _Rest, _Role) ->
                {ok, {"WHATVER", "SECRET", "2038-05-04T10:12:13Z"}};
           ("close_expire" ++ _Rest, _Role) ->
                Timestamp = calendar:gregorian_seconds_to_datetime(
                    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) +
                    ?EXPIRATION_REFRESH - 1),
                {ok, {"WHATVER", "SECRET", kinetic_iso8601:format(Timestamp)}}

        end
    ),
    meck:new(kinetic_utils, [passthrough]),
    meck:expect(kinetic_utils, fetch_and_return_url,
                fun(_MetaData, text) -> {ok, "us-east-1b"} end).

test_teardown(_) ->
    meck:unload(kinetic_iam),
    meck:unload(kinetic_utils).

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
                                         {aws_secret_access_key, "secret"},
                                         {metadata_base_url, "doesn't matter"}]),
    {ok, #kinetic_arguments{
        aws_credentials=#aws_credentials{
            access_key_id="whatever",
            secret_access_key="secret",
            expiration_seconds=no_expire
        },
        region="us-east-1",
        lhttpc_opts=[]}} = kinetic_config:get_args(),

    kinetic_sup:stop(Pid),
    {error, _} = kinetic_config:get_args(),
    process_flag(trap_exit, false).

