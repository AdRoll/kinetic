-module(kinetic_stream).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([stop/0, start_link/1, put_record/2]).

-include("kinetic.hrl").

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

stop() ->
    gen_server:call(?MODULE, stop).


put_record(Config, Data) ->
    Stream = get_stream(Config),
    gen_server:call(Stream, {put_record, Data}, infinity).

% gen_server behavior
init({StreamName, BasePartitionName, PartitionsNumber, Timeout}) ->
    process_flag(trap_exit, true),
    case ets:insert_new(?MODULE, StreamName, self()) of
        true ->
            {ok, #kinetic_stream{stream_name=StreamName,
                                 base_partition_name=BasePartitionName,
                                 partitions_number=PartitionsNumber,
                                 timeout=Timeout,
                                 buffer= <<"">>,
                                 buffer_size=0,
                                 current_partition_num=0}};
        false ->
            ignore
    end.


handle_call({put_record, Data}, _From, State=#kinetic_stream{buffer=Buffer, buffer_size=BSize}) ->
    % Data is bigger than ?KINESIS_MAX_PUT_SIZE
    % buffer + Data is bigger than (or equal to) ?KINESIS_MAX_PUT_SIZE
    % buffer + Data is not bigger than ?KINESIS_MAX_PUT_SIZE
    DataSize = erlang:size(Data),
    case DataSize > ?KINESIS_MAX_PUT_SIZE of
        true ->
            {reply, {error, max_size_exceeded}, State};

        false ->
            case BSize + DataSize > ?KINESIS_MAX_PUT_SIZE of
                true ->
                    NewState = internal_flush(State),
                    {reply, ok, NewState#kinetic_stream{buffer_size=DataSize,
                                                        buffer=Data}};
                false ->
                    {reply, ok, State#kinetic_stream{buffer= <<Buffer/binary, Data/binary>>,
                                                     buffer_size=BSize+DataSize}}
            end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Arg, State) ->
    {noreply, State}.

terminate(_Reason, #kinetic_stream{stream_name=StreamName}) ->
    ets:delete(?MODULE, StreamName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info(_Info, State) ->
    {noreply, State}.

% Internal implementation
get_stream(Config={StreamName, _BasePartitionName, _PartitionsNumber, _Timeout}) ->
    case ets:lookup(?MODULE, StreamName) of
        [] ->
            case supervisor:start_child(kinetic_stream_sup, [Config]) of
                {ok, undefined} -> get_stream(Config);
                {ok, Pid} -> Pid
            end;
        [{_Name, Pid}] ->
            case is_process_alive(Pid) of
                true -> Pid;
                false ->
                    ets:delete(?MODULE, StreamName),
                    get_stream(Config)
            end
    end.

internal_flush(State=#kinetic_stream{stream_name=StreamName, buffer=Buffer}) ->
    PartitionKey = partition_key(State),
    kinetic:put_record([{<<"Data">>, base64:encode(Buffer)},
                        {<<"PartitionKey">>, PartitionKey},
                        {<<"StreamName">>, StreamName}]),
    increment_partition_num(State#kinetic_stream{buffer= <<"">>, buffer_size=0}).

increment_partition_num(State=#kinetic_stream{current_partition_num=Number,
                                              partitions_number=Number}) ->
    State#kinetic_stream{current_partition_num=0};
increment_partition_num(State=#kinetic_stream{current_partition_num=Number}) ->
    State#kinetic_stream{current_partition_num=Number+1}.

partition_key(#kinetic_stream{current_partition_num=Number, base_partition_name=BasePartitionName}) ->
    BinNumber = integer_to_binary(Number),
    <<BasePartitionName/binary, "-", BinNumber/binary>>.

