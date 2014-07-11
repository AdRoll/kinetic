-module(kinetic_config_tests).

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
                fun(_MetaData, text) -> {ok, "us-east-1b"} end),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, apply_interval, fun
            (Interval, M, F, [Opts]) ->
                case proplists:get_value(should_err, Opts) of
                    true ->
                        {error, broken};
                    _ ->
                        meck:passthrough([Interval, M, F, [Opts]])
                end
        end).

test_teardown(_) ->
    meck:unload(timer),
    meck:unload(kinetic_iam),
    meck:unload(kinetic_utils).

test_setup_ets() ->
    test_setup(),
    ets:new(?KINETIC_DATA, [named_table, set, public, {read_concurrency, true}]).

test_teardown_ets(_V) ->
    test_teardown(_V),
    ets:delete(?KINETIC_DATA).

kinetic_config_test_() ->
    {inorder,
        {foreach,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?_test(test_passed_metadata()),
                ?_test(test_config_env()),
                ?_test(test_error_init())
            ]
        }
    }.

kinetic_config_ets_test_() ->
    {inorder,
        {foreach,
            fun test_setup_ets/0,
            fun test_teardown_ets/1,
            [
                ?_test(test_update_data())
            ]
        }
    }.

test_config_env() ->
    application:set_env(kinetic, whatever, value),
    value = kinetic_config:g(whatever),
    undefined = kinetic_config:g(something).

test_error_init() ->
    process_flag(trap_exit, true),
    {error, {error, broken}} = kinetic_config:start_link([{metadata_base_url, "no_expire"}, {should_err, true}]),
    process_flag(trap_exit, false).

test_passed_metadata() ->
    {ok, _Pid} = kinetic_config:start_link([{aws_access_key_id, "whatever"},
                                            {aws_secret_access_key, "secret"},
                                            {metadata_base_url, "doesn't matter"}]),
    ?assert(ets:info(?KINETIC_STREAM) =/= undefined),
    {ok, #kinetic_arguments{access_key_id="whatever",
                            secret_access_key="secret",
                            region="us-east-1",
                            expiration_seconds=no_expire,
                            lhttpc_opts=[]}} = kinetic_config:get_args(),
    kinetic_config:update_data([{aws_access_key_id, "whatever"},
                                {aws_secret_access_key, "secret"},
                                {metadata_base_url, "doesn't matter"}]),
    {ok, #kinetic_arguments{access_key_id="whatever",
                            secret_access_key="secret",
                            region="us-east-1",
                            expiration_seconds=no_expire,
                            lhttpc_opts=[]}} = kinetic_config:get_args(),
    kinetic_config:stop(),
    {error, _} = kinetic_config:get_args(),
    undefined = ets:info(?KINETIC_STREAM).

test_update_data() ->
    {ok, #kinetic_arguments{access_key_id="WHATVER",
                            secret_access_key="SECRET",
                            region="us-east-1",
                            expiration_seconds=_Expire,
                            date=_Date}} = kinetic_config:update_data([{metadata_base_url, "close_expire"}]),
    {ok, #kinetic_arguments{access_key_id="WHATVER",
                            secret_access_key="SECRET",
                            region="us-east-1",
                            expiration_seconds=64323799933,
                            date=_Date3}} = kinetic_config:update_data([{metadata_base_url, "no_expire"}]),
    {ok, #kinetic_arguments{access_key_id="WHATVER",
                            secret_access_key="SECRET",
                            region="us-east-1",
                            expiration_seconds=64323799933,
                            date=_Date3}} = kinetic_config:update_data([{metadata_base_url, "no_expire"}]).



