-module(kinetic).
-behaviour(application).


-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([start/1]).


-export([create_stream/1, create_stream/2]).
-export([list_streams/1, list_streams/2]).
-export([delete_stream/1, delete_stream/2]).
-export([describe_stream/1, describe_stream/2]).
-export([get_records/1, get_records/2]).
-export([get_shard_iterator/1, get_shard_iterator/2]).
-export([merge_shards/1, merge_shards/2]).
-export([put_record/1, put_record/2]).
-export([split_shard/1, split_shard/2]).

-include("kinetic.hrl").
-define(DEFAULT_TIMEOUT, 5000).

% application behaviour

-spec start() -> ok | {error, any()}.
start() ->
    application:start(kinetic).


-spec stop() -> ok | {error, any()}.
stop() ->
    application:stop(kinetic).


-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.
start(Opts) when is_list(Opts) ->
    kinetic_sup:start_link(Opts).

start(_, Opts) when is_list(Opts) ->
    kinetic_sup:start_link(Opts);
start(_, _) ->
    kinetic_sup:start_link().

-spec stop(any()) -> ok.
stop(_) ->
    ok.

% Public API

%%
%% Payload = [{<<"ShardCount">>, binary()}, <- required
%%            {<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, []}
create_stream(Payload) ->
    create_stream(Payload, ?DEFAULT_TIMEOUT).
create_stream(Payload, Timeout) ->
    execute("CreateStream", Payload, Timeout).

%%
%% Payload = [{<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, []}
delete_stream(Payload) ->
    delete_stream(Payload, ?DEFAULT_TIMEOUT).
delete_stream(Payload, Timeout) ->
    execute("DeleteStream", Payload, Timeout).

%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"Limit">>, binary()},      <- optional
%%            {<<"ExclusiveStartShardId">>, binary()}] <- optional
%%
%% Response = {ok, [{<<"StreamDescription">>,
%%   {[{<<"HasMoreShards">>,false},
%%     {<<"StreamStatus">>,<<"ACTIVE">>},
%%     {<<"StreamName">>,<<"exampleStreamName">>},
%%     {<<"StreamARN">>,
%%      <<"arn:aws:kinesis:us-east-1:052958737983:exampleStreamName">>},
%%     {<<"Shards">>,
%%      [{[{<<"HashKeyRange">>,
%%          {[{<<"EndingHashKey">>,
%%             <<"113427455640312821154458202477256070484">>},
%%            {<<"StartingHashKey">>,<<"0">>}]}},
%%         {<<"ShardId">>,<<"shardId-000000000000">>},
%%         {<<"SequenceNumberRange">>,
%%          {[{<<"EndingSequenceNumber">>,
%%             <<"21269319989741826081360214168359141376">>},
%%            {<<"StartingSequenceNumber">>,
%%             <<"21267647932558653966460912964485513216">>}]}}]},
%%       {[{<<"HashKeyRange">>,
%%          {[{<<"EndingHashKey">>,
%%             <<"226854911280625642308916404954512140969">>},
%%            {<<"StartingHashKey">>,
%%             <<"113427455640312821154458202477256070485">>}]}},
%%         {<<"ShardId">>,<<"shardId-000000000001">>},
%%         {<<"SequenceNumberRange">>,
%%          {[{<<"StartingSequenceNumber">>,
%%             <<"21267647932558653966460912964485513217">>}]}}]},
%%       {[{<<"HashKeyRange">>,
%%          {[{<<"EndingHashKey">>,
%%             <<"340282366920938463463374607431768211455">>},
%%            {<<"StartingHashKey">>,
%%             <<"226854911280625642308916404954512140970">>}]}},
%%         {<<"ShardId">>,<<"shardId-000000000002">>},
%%         {<<"SequenceNumberRange">>,
%%          {[{<<"StartingSequenceNumber">>,
%%             <<"21267647932558653966460912964485513218">>}]}}]}]}]}}]
describe_stream(Payload) ->
    describe_stream(Payload, ?DEFAULT_TIMEOUT).
describe_stream(Payload, Timeout) ->
    execute("DescribeStream", Payload, Timeout).

%%
%% Payload = [{<<"Limit">>, binary()}, <- optional
%%            {<<"ShardIterator">>, binary()}] <- required
%% 
%% Response = {ok, [
%%  {<<"NextShardIterator">>, <<"AAAAAAAAAAHsW8zCWf9164uy8Epue6WS3w6wmj4a4USt+CNvMd6uXQ+HL5vAJMznqqC0DLKsIjuoiTi1BpT6nW0LN2M2D56zM5H8anHm30Gbri9ua+qaGgj+3XTyvbhpERfrezgLHbPB/rIcVpykJbaSj5tmcXYRmFnqZBEyHwtZYFmh6hvWVFkIwLuMZLMrpWhG5r5hzkE=">>},
%%  {<<"Records">>, [{<<"Data">>, <<"XzxkYXRhPl8w">>},
%%                   {<<"PartitionKey">>, <<"partitionKey">>},
%%                   {<<"SequenceNumber">>: <<"21269319989652663814458848515492872193">>}]}]}
get_records(Payload) ->
    get_records(Payload, ?DEFAULT_TIMEOUT).
get_records(Payload, Timeout) ->
    execute("GetRecords", Payload, Timeout).

%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"ShardId">>, binary()},    <- required
%%            {<<"ShardIteratorType">>, <<"AT_SEQUENCE_NUMBER | AFTER_SEQUENCE_NUMBER | TRIM_HORIZON | LATEST">>}, <- required
%%            {<<"StartingSequenceNumber">>, binary()}] <- optional
%% 
%% Response = {ok, [
%%  {<<"ShardIterator">>, <<"AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY=">>}
%% ]}
get_shard_iterator(Payload) ->
    get_shard_iterator(Payload, ?DEFAULT_TIMEOUT).
get_shard_iterator(Payload, Timeout) ->
    execute("GetShardIterator", Payload, Timeout).

%%
%% Payload = [{<<"ExclusiveStartStreamName">>, binary()}, <- optional
%%            {<<"Limit">>, binary()}] <- optional
%% 
%% Response = {ok, [{<<"HasMoreStreams">>, false},
%%                  {<<"StreamNames">>, [<<"exampleStreamName">>]}]}
list_streams(Payload) ->
    list_streams(Payload, ?DEFAULT_TIMEOUT).
list_streams(Payload, Timeout) ->
    execute("ListStreams", Payload, Timeout).

%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"ShardToMerge">>, binary()},      <- required
%%            {<<"AdjacentShardToMerge">>, binary()}] <- required
%%
%% Response = {ok, []}
merge_shards(Payload) ->
    merge_shards(Payload, ?DEFAULT_TIMEOUT).
merge_shards(Payload, Timeout) ->
    execute("MergeShards", Payload, Timeout).

%%
%% Payload = [{<<"Data">>, binary()}, <- required
%%            {<<"ExplicitHashKey">>, binary()},    <- optional
%%            {<<"PartitionKey">>, binary()}, <- required
%%            {<<"SequenceNumberForOrdering">>, binary()}, <- optional
%%            {<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, [{<<"SequenceNumber">>, <<"21269319989653637946712965403778482177">>},
%%                  {<<"ShardId">>, <<"shardId-000000000001">>}]}
put_record(Payload) ->
    put_record(Payload, ?DEFAULT_TIMEOUT).
put_record(Payload, Timeout) ->
    execute("PutRecord", Payload, Timeout).

%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"ShardToSplit">>, binary()},      <- required
%%            {<<"NewStartingHashKey">>, binary()}] <- required
%% 
%% Response = {ok, []}
split_shard(Payload) ->
    split_shard(Payload, ?DEFAULT_TIMEOUT).
split_shard(Payload, Timeout) ->
    execute("SplitShard", Payload, Timeout).

%% Internal
execute(Operation, Payload, Timeout) ->
    case get_args() of
        {error, E} ->
            throw(E);

        {ok, #kinetic_arguments{access_key_id=AccessKeyId, secret_access_key=SecretAccessKey,
                                    region=Region, date=Date, host=Host, url=Url,
                                    lhttpc_opts=LHttpcOpts}} ->
            Target = ["Kinesis_20131202.", Operation],
            Body = jiffy:encode({Payload}),
            {ok, AuthorizationHeader} = kinetic_aws:sign_v4(AccessKeyId, SecretAccessKey, "kinesis",
                                                      Region, Date, Target, Body),
            Headers = [{"Content-Type", "application/x-amz-json-1.1"},
                       {"Connection", "keep-alive"},
                       {"x-amz-target", Target},
                       {"x-amz-date", Date},
                       {"Host", Host},
                       {"Authorization", AuthorizationHeader}],
            case lhttpc:request(Url, post, Headers, Body, Timeout, LHttpcOpts) of
                {ok, {{200, _}, _ResponseHeaders, ResponseBody}} ->
                    {ok, unpack(ResponseBody)};

                {ok, {{Code, _}, ResponseHeaders, ResponseBody}} ->
                    {error, Code, ResponseHeaders, ResponseBody}
            end
    end.

get_args() ->
    case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {error, missing_credentials};
        V ->
            {ok, V}
    end.

unpack(<<"">>) ->
    [];
unpack("") ->
    [];
unpack(Body) ->
    {Decoded} = jiffy:decode(Body),
    Decoded.

