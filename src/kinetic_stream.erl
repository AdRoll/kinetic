-module(kinetic_stream).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([stop/2, start_link/2, put_record/3]).
-export([flush/2]).


% I dislike this soooo much
-ifdef(TEST).
-export([get_stream/2, send_to_kinesis/5]).
-endif.

-include("kinetic.hrl").

start_link(StreamName, Config) ->
    gen_server:start_link(?MODULE, [StreamName, Config], []).

stop(StreamName, Config) ->
    Stream = get_stream(StreamName, Config),
    gen_server:call(Stream, stop).


put_record(StreamName, Config, Data) ->
    DataSize = erlang:size(Data),
    case DataSize > ?KINESIS_MAX_PUT_SIZE of
        true ->
            {error, max_size_exceeded};
        false ->
            Stream = get_stream(StreamName, Config),
            gen_server:call(Stream, {put_record, Data, DataSize}, infinity)
    end.

flush(StreamName, Config) ->
    Stream = get_stream(StreamName, Config),
    Stream ! flush.

% gen_server behavior
init([StreamName, {BasePartitionName}]) ->
    init([StreamName, {BasePartitionName, 1000}]);
init([StreamName, {BasePartitionName, PartitionsNumber}]) ->
    init([StreamName, {BasePartitionName, PartitionsNumber, 3}]);
init([StreamName, {BasePartitionName, PartitionsNumber, Retries}]) ->
    init([StreamName, {BasePartitionName, PartitionsNumber, Retries, 5000}]);
init([StreamName, {BasePartitionName, PartitionsNumber, Retries, Timeout}]) ->
    init([StreamName, {BasePartitionName, PartitionsNumber, Retries, Timeout, 1000}]);
init([StreamName, {BasePartitionName, PartitionsNumber, Retries, Timeout, FlushInterval}]) ->
    process_flag(trap_exit, true),
    case ets:insert_new(?KINETIC_STREAM, {StreamName, self()}) of
        true ->
            {ok, TRef} = timer:send_after(FlushInterval, self(), flush),
            {ok, #kinetic_stream{stream_name=StreamName,
                                 base_partition_name=BasePartitionName,
                                 partitions_number=PartitionsNumber,
                                 timeout=Timeout,
                                 buffer= <<"">>,
                                 buffer_size=0,
                                 current_partition_num=0,
                                 flush_interval=FlushInterval,
                                 flush_tref=TRef,
                                 retries=Retries}};
        false ->
            ignore
    end.


% buffer + Data is bigger than (or equal to) ?KINESIS_MAX_PUT_SIZE
% buffer + Data is not bigger than ?KINESIS_MAX_PUT_SIZE
handle_call({put_record, Data, DataSize}, _From,
            State=#kinetic_stream{buffer_size=BSize})
        when BSize + DataSize > ?KINESIS_MAX_PUT_SIZE ->
    NewState = internal_flush(State),
    {reply, ok, reset_timer(NewState#kinetic_stream{buffer_size=DataSize, buffer=Data})};
handle_call({put_record, Data, DataSize}, _From,
            State=#kinetic_stream{buffer=Buffer, buffer_size=BSize}) ->
    {reply, ok, reset_timer(State#kinetic_stream{
                buffer= <<Buffer/binary, Data/binary>>,
                buffer_size=BSize+DataSize})};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Arg, State) ->
    {noreply, State}.

terminate(_Reason, #kinetic_stream{stream_name=StreamName, flush_tref=Tref}) ->
    ets:delete(?MODULE, StreamName),
    timer:cancel(Tref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info(flush, State) ->
    NewState = internal_flush(State),
    {noreply, reset_timer(NewState)};
handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) ->
    error_logger:info_msg("kinetic_stream: ~p exited due to: ~p~n", [From, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% Internal implementation
get_stream(StreamName, Config) ->
    case ets:lookup(?KINETIC_STREAM, StreamName) of
        [] ->
            case supervisor:start_child(kinetic_stream_sup, [StreamName, Config]) of
                {ok, undefined} -> get_stream(StreamName, Config);
                {ok, Pid} -> Pid
            end;
        [{_Name, Pid}] ->
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete(?KINETIC_STREAM , StreamName),
                    get_stream(StreamName, Config)
            end
    end.

internal_flush(State=#kinetic_stream{buffer= <<"">>}) ->
    State;
internal_flush(State=#kinetic_stream{stream_name=StreamName,
                                     buffer=Buffer,
                                     timeout=Timeout,
                                     retries=Retries}) ->
    PartitionKey = partition_key(State),
    spawn_link(fun() -> send_to_kinesis(StreamName, Buffer, PartitionKey, Timeout, Retries+1) end),
    increment_partition_num(State#kinetic_stream{buffer= <<"">>, buffer_size=0}).

increment_partition_num(State=#kinetic_stream{current_partition_num=Number,
                                              partitions_number=Number}) ->
    State#kinetic_stream{current_partition_num=0};
increment_partition_num(State=#kinetic_stream{current_partition_num=Number}) ->
    State#kinetic_stream{current_partition_num=Number+1}.

partition_key(
    #kinetic_stream{current_partition_num=Number, base_partition_name=BasePartitionName}) ->
    BinNumber = integer_to_binary(Number),
    <<BasePartitionName/binary, "-", BinNumber/binary>>.

reset_timer(State=#kinetic_stream{flush_interval=FlushInterval, flush_tref=TRef}) ->
    timer:cancel(TRef),
    {ok, NewTRef} = timer:send_after(FlushInterval, self(), flush),
    State#kinetic_stream{flush_tref=NewTRef}.

send_to_kinesis(StreamName, Buffer, PartitionKey, Timeout, 0) ->
    erlang:error(max_retries_reached, [StreamName, PartitionKey, Timeout, Buffer]);
send_to_kinesis(StreamName, Buffer, PartitionKey, Timeout, Retries) ->
    case kinetic:put_record([{<<"Data">>, b64fast:encode64(Buffer)},
                             {<<"PartitionKey">>, PartitionKey},
                             {<<"StreamName">>, StreamName}], Timeout) of
        {ok, _} ->
            {ok, done};

        {error, {Code, Headers, RawBody}} ->
            Body = kinetic_utils:decode(RawBody),
            case proplists:get_value(<<"__type">>, Body) of
                <<"ProvisionedThroughputExceededException">> ->
                    timer:sleep(1000), % not really exponential
                    send_to_kinesis(StreamName, Buffer, PartitionKey, Timeout, Retries-1);

                _ ->
                    error_logger:info_msg(
                        "Request failed: Code: ~p~n~n~p~n~p~n", [Code, Headers, RawBody]),
                    {error, {Code, Headers, Body}}
            end
    end.


