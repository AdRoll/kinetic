-module(kinetic_stream_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    ets:new(?KINETIC_STREAM, [named_table, set, public, {read_concurrency, true}]),
    meck:new(supervisor, [unstick, passthrough]),
    meck:sequence(supervisor, start_child, 2, [{ok, undefined}, {ok, pid}]),
    meck:new(kinetic, [passthrough]),
    meck:expect(kinetic, put_record,
        fun(Payload, _Timeout) ->
                case proplists:get_value(<<"error">>, Payload) of
                    undefined ->
                        self() ! done,
                        {ok, done};

                    <<"throughput">> ->
                        {error, 400, headers, <<"{\"__type\": \"ProvisionedThroughputExceededException\"}">>};

                    _ ->
                        {error, 400, headers, <<"{\"__type\": \"OtherStuff\"}">>}
                end
        end),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, send_after, fun
            (1000, _pid, flush) ->
                {ok, tref}
        end),
    meck:expect(timer, sleep, fun
            (1000) ->
                ok
        end),
    meck:expect(timer, cancel, fun
            (tref) ->
                ok
        end).

test_teardown(_) ->
    ets:delete(?KINETIC_STREAM),
    meck:unload(timer),
    meck:unload(kinetic),
    meck:unload(supervisor).

kinetic_config_test_() ->
    {inorder,
        {foreach,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?_test(test_get_stream()),
                ?_test(test_start_and_stop()),
                ?_test(test_functionality())
            ]
        }
    }.


%%
%% Tests
%%
test_get_stream() ->
    Pid = self(),
    pid = kinetic_stream:get_stream(<<"mystream">>, {<<"whatever">>}),
    ets:insert_new(?KINETIC_STREAM, {<<"mystream">>, self()}),
    Pid = kinetic_stream:get_stream(<<"mystream">>, {<<"whatever">>}),
    ets:delete(?KINETIC_STREAM, <<"mystream">>),

    ChildPid = spawn(fun() -> Pid ! done end),
    ok = receive
        done ->
            ok;
        _ ->
            bad
         after
            1000 ->
              bad
    end,
    ets:insert_new(?KINETIC_STREAM, {<<"mystream">>, ChildPid}),
    pid = kinetic_stream:get_stream(<<"mystream">>, {<<"whatever">>}).



test_start_and_stop() ->
    {ok, Pid} = kinetic_stream:start_link(<<"mystream">>, {<<"whatever">>}),
    Pid = kinetic_stream:get_stream(<<"mystream">>, {<<"whatever">>}),
    true = meck:called(timer, send_after, [1000, Pid, flush]),
    kinetic_stream:flush(<<"mystream">>, {<<"whatever">>}),
    kinetic_stream:stop(<<"mystream">>, {<<"whatever">>}),
    2 = meck:num_calls(timer, cancel, [tref]),
    false = meck:called(kinetic, put_record, ['_', '_']),
    ok.

test_functionality() ->
    BigData = list_to_binary(string:chars($a, ?KINESIS_MAX_PUT_SIZE+1)),
    SmallData = <<"data">>,
    _RegularData = list_to_binary(string:chars($a, ?KINESIS_MAX_PUT_SIZE-1)),
    S = <<"mystream">>,
    P = <<"whatever">>,
    {ok, _Pid} = kinetic_stream:start_link(S, {P}),
    {error, max_size_exceeded} = kinetic_stream:put_record(S, {P}, BigData),
    ok = kinetic_stream:put_record(S, {P}, SmallData),
    kinetic_stream:flush(S, {P}),
    Payload0 = [{<<"Data">>, base64:encode(SmallData)},
                {<<"PartitionKey">>, <<P/binary, "-0">>},
                {<<"StreamName">>, S}],
    receive
        done ->
            ok;
        _ ->
            bad
    after
        1000 ->
            bad
    end,
    true = meck:called(kinetic, put_record, [Payload0, 5000]),
    ok.

