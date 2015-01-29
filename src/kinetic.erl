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

start(_, Opts) ->
    kinetic_sup:start_link(Opts).

-spec stop(any()) -> ok.
stop(_) ->
    ok.

% Public API

%%
%% Payload = [{<<"ShardCount">>, integer()}, <- required
%%            {<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, []}
create_stream(Payload) ->
    create_stream(Payload, []).
create_stream(Payload, Opts) when is_list(Opts) ->
    execute("CreateStream", Payload, Opts);
create_stream(Payload, Timeout) ->
    create_stream(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, []}
delete_stream(Payload) ->
    delete_stream(Payload, []).
delete_stream(Payload, Opts) when is_list(Opts) ->
    execute("DeleteStream", Payload, Opts);
delete_stream(Payload, Timeout) ->
    delete_stream(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"Limit">>, integer()},      <- optional
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
    describe_stream(Payload, []).
describe_stream(Payload, Opts) when is_list(Opts) ->
    execute("DescribeStream", Payload, Opts);
describe_stream(Payload, Timeout) ->
    describe_stream(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"Limit">>, integer()}, <- optional
%%            {<<"ShardIterator">>, binary()}] <- required
%% 
%% Response = {ok, [
%%  {<<"NextShardIterator">>, <<"AAAAAAAAAAHsW8zCWf9164uy8Epue6WS3w6wmj4a4USt+CNvMd6uXQ+HL5vAJMznqqC0DLKsIjuoiTi1BpT6nW0LN2M2D56zM5H8anHm30Gbri9ua+qaGgj+3XTyvbhpERfrezgLHbPB/rIcVpykJbaSj5tmcXYRmFnqZBEyHwtZYFmh6hvWVFkIwLuMZLMrpWhG5r5hzkE=">>},
%%  {<<"Records">>, [{<<"Data">>, <<"XzxkYXRhPl8w">>},
%%                   {<<"PartitionKey">>, <<"partitionKey">>},
%%                   {<<"SequenceNumber">>: <<"21269319989652663814458848515492872193">>}]}]}
get_records(Payload) ->
    get_records(Payload, []).
get_records(Payload, Opts) when is_list(Opts) ->
    execute("GetRecords", Payload, Opts);
get_records(Payload, Timeout) ->
    get_records(Payload, [{timeout, Timeout}]).


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
    get_shard_iterator(Payload, []).
get_shard_iterator(Payload, Opts) when is_list(Opts) ->
    execute("GetShardIterator", Payload, Opts);
get_shard_iterator(Payload, Timeout) ->
    get_shard_iterator(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"ExclusiveStartStreamName">>, binary()}, <- optional
%%            {<<"Limit">>, integer()}] <- optional
%% 
%% Response = {ok, [{<<"HasMoreStreams">>, false},
%%                  {<<"StreamNames">>, [<<"exampleStreamName">>]}]}
list_streams(Payload) ->
    list_streams(Payload, []).
list_streams(Payload, Opts) when is_list(Opts) ->
    execute("ListStreams", Payload, Opts);
list_streams(Payload, Timeout) ->
    list_streams(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"ShardToMerge">>, binary()},      <- required
%%            {<<"AdjacentShardToMerge">>, binary()}] <- required
%%
%% Response = {ok, []}
merge_shards(Payload) ->
    merge_shards(Payload, []).
merge_shards(Payload, Opts) when is_list(Opts) ->
    execute("MergeShards", Payload, Opts);
merge_shards(Payload, Timeout) ->
    merge_shards(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"Data">>, base64_binary()}, <- required
%%            {<<"ExplicitHashKey">>, binary()},    <- optional
%%            {<<"PartitionKey">>, binary()}, <- required
%%            {<<"SequenceNumberForOrdering">>, binary()}, <- optional
%%            {<<"StreamName">>, binary()}] <- required
%% 
%% Response = {ok, [{<<"SequenceNumber">>, <<"21269319989653637946712965403778482177">>},
%%                  {<<"ShardId">>, <<"shardId-000000000001">>}]}
put_record(Payload) ->
    put_record(Payload, []).
put_record(Payload, Opts) when is_list(Opts) ->
    execute("PutRecord", Payload, Opts);
put_record(Payload, Timeout) ->
    put_record(Payload, [{timeout, Timeout}]).


%%
%% Payload = [{<<"StreamName">>, binary()}, <- required
%%            {<<"ShardToSplit">>, binary()},      <- required
%%            {<<"NewStartingHashKey">>, binary()}] <- required
%% 
%% Response = {ok, []}
split_shard(Payload) ->
    split_shard(Payload, []).
split_shard(Payload, Opts) when is_list(Opts) ->
    execute("SplitShard", Payload, Opts);
split_shard(Payload, Timeout) ->
    split_shard(Payload, [{timeout, Timeout}]).


%% Internal
execute(Operation, Payload, Opts) ->
    case kinetic_config:get_args() of
        {error, E} ->
            {error, E};

        {ok, Args} ->
            #kinetic_arguments{aws_credentials=AwsCreds, region=Region, date=Date, url=Url,
                               lhttpc_opts=LHttpcOpts, timeout=Timeout} = kinetic_config:merge_args(Args, Opts),
            case kinetic_utils:encode({Payload}) of
                {error, E} ->
                    {error, E};

                Body ->
                    Target = ["Kinesis_20131202.", Operation],

                    {ok, AuthorizationHeaders} =
                        kinetic_aws:authorization_headers_v4(AwsCreds, "kinesis", Region,
                                                          Date, Target, Body),

                    Headers = [{"Content-Type", "application/x-amz-json-1.1"},
                               {"Connection", "keep-alive"}
                               | AuthorizationHeaders],

                    case lhttpc:request(Url, post, Headers, Body, Timeout, LHttpcOpts) of
                        {ok, {{200, _}, _ResponseHeaders, ResponseBody}} ->
                            {ok, kinetic_utils:decode(ResponseBody)};

                        {ok, {{Code, _}, ResponseHeaders, ResponseBody}} ->
                            {error, Code, ResponseHeaders, ResponseBody};

                        {error, E} ->
                            {error, E}
                    end
            end
    end.


