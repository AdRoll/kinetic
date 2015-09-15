-module(kinetic_config).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([start_link/1, update_data/1, stop/0, g/1, get_args/0, merge_args/2]).

-include("kinetic.hrl").

-record(kinetic_config, {tref}).

-define(METADATA_BASE_URL, "http://169.254.169.254").

start_link(Opts) ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, [Opts], []).

stop() ->
    gen_server:call(?MODULE, stop).

g(Name) ->
    case application:get_env(kinetic, Name) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.

get_args() ->
    try ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2) of
        V ->
            {ok, V}
    catch
        error:badarg ->
            {error, missing_credentials}
    end.

update_data(Opts) ->
    Arguments = case get_args() of
        {error, missing_credentials} ->
            new_args(Opts);
        {ok, Result} ->
            update_data_subsequent(Opts, Result)
    end,
    ets:insert(?KINETIC_DATA, {?KINETIC_ARGS_KEY, Arguments}),
    {ok, Arguments}.

% gen_server behavior

init([Opts]) ->
    process_flag(trap_exit, true),
    ets:new(?KINETIC_DATA, [named_table, set, public, {read_concurrency, true}]),
    ets:new(?KINETIC_STREAM, [named_table, set, public, {read_concurrency, true}]),
    {ok, _ClientArgs} = update_data(Opts),
    case timer:apply_interval(1000, ?MODULE, update_data, [Opts]) of
        {ok, TRef} ->
            {ok, #kinetic_config{tref=TRef}};
        Error ->
            {stop, Error}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Arg, State) ->
    {noreply, State}.

terminate(_Reason, _State=#kinetic_config{tref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    true = ets:delete(?KINETIC_DATA),
    true = ets:delete(?KINETIC_STREAM),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) ->
    error_logger:info_msg("kinetic_config: ~p exited due to: ~p~n", [From, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% Internal implementation

% -spec region(zone()) -> region().
region("us-east-1" ++ _R) -> "us-east-1";
region("us-west-1" ++ _R) -> "us-west-1";
region("us-west-2" ++ _R) -> "us-west-2";
region("ap-northeast-1" ++ _R) -> "ap-northeast-1";
region("ap-southeast-1" ++ _R) -> "ap-southeast-1";
region("eu-west-1" ++ _R) -> "eu-west-1".

get_aws_credentials(V, P, MetaData, Role)
        when V =:= undefined orelse P =:= undefined ->
    {ok, AwsCredentials} = kinetic_iam:get_aws_keys(MetaData, Role),
    AwsCredentials;
get_aws_credentials(AccessKeyId, SecretAccessKey, _, _) when is_list(AccessKeyId), is_list(SecretAccessKey) ->
    #aws_credentials{
        access_key_id = AccessKeyId,
        secret_access_key = SecretAccessKey,
        expiration_seconds = no_expire
    }.


update_data_subsequent(Opts, Args=#kinetic_arguments{aws_credentials = AwsCreds}) ->
    case AwsCreds of
        #aws_credentials{expiration_seconds=no_expire} ->
            Args#kinetic_arguments{date=isonow()};
        #aws_credentials{expiration_seconds=CurrentExpirationSeconds} ->
            SecondsToExpire = CurrentExpirationSeconds - calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
            case SecondsToExpire < ?EXPIRATION_REFRESH of
                true ->
                    new_args(Opts);
                false ->
                    Args#kinetic_arguments{date=isonow()}
            end
    end.


new_args(Opts) ->
    ConfiguredAccessKeyId = proplists:get_value(aws_access_key_id, Opts),
    ConfiguredSecretAccessKey = proplists:get_value(aws_secret_access_key, Opts),
    MetaData = proplists:get_value(metadata_base_url, Opts, ?METADATA_BASE_URL),

    Region = case proplists:get_value(region, Opts, undefined) of
                 undefined ->
                     {ok, Zone} = kinetic_utils:fetch_and_return_url(MetaData ++ "/latest/meta-data/placement/availability-zone", text),
                     region(Zone);
                 R ->
                     R
             end,

    LHttpcOpts = proplists:get_value(lhttpc_opts, Opts, []),
    DefaultTimeout = proplists:get_value(timeout, Opts, ?DEFAULT_OPERATION_TIMEOUT),
    Host = kinetic_utils:endpoint("kinesis", Region),
    Url = "https://" ++ Host,
    Role = proplists:get_value(iam_role, Opts),

    #kinetic_arguments{
        region=Region,
        date=isonow(),
        host=Host,
        url=Url,
        lhttpc_opts=LHttpcOpts,
        timeout=DefaultTimeout,
        aws_credentials = get_aws_credentials(ConfiguredAccessKeyId,
                                              ConfiguredSecretAccessKey,
                                              MetaData, Role)
    }.


%% todo:
%% - rewrite new_args to use this
%% - handle additional args
merge_args(Args, []) ->
    Args;
merge_args(Args, [{region, Region}|Rest]) ->
    Host = kinetic_utils:endpoint("kinesis", Region),
    Url = "https://" ++ Host,
    merge_args(Args#kinetic_arguments{region = Region,
                                      host = Host,
                                      url = Url},
               Rest);
merge_args(Args, [{timeout, Timeout}|Rest]) ->
    merge_args(Args#kinetic_arguments{timeout = Timeout}, Rest).



isonow() ->
    kinetic_iso8601:format_basic(erlang:universaltime()).
