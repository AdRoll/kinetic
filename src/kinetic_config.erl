-module(kinetic_config).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).
-export([start_link/1, update_data/1, stop/0, g/1, get_args/0, merge_args/2]).

-include("kinetic.hrl").

-record(kinetic_config, {tref}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

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
    try ets:lookup_element(?KINETIC_DATA, args, 2) of
        V ->
            {ok, V}
    catch
        error:badarg ->
            {error, missing_args}
    end.

update_data(Opts) ->
    Arguments =
        case get_args() of
            {error, missing_args} ->
                new_args(Opts);
            {ok, Result} ->
                Result#kinetic_arguments{aws_credentials = erliam:credentials(),
                                         date = awsv4:isonow()}
        end,
    ets:insert(?KINETIC_DATA, {args, Arguments}),
    {ok, Arguments}.

% gen_server behavior

init([Opts]) ->
    process_flag(trap_exit, true),
    EtsOpts = [named_table, set, public, {read_concurrency, true}],
    ets:new(?KINETIC_DATA, EtsOpts),
    ets:new(?KINETIC_STREAM, EtsOpts),
    {ok, _ClientArgs} = update_data(Opts),
    case timer:apply_interval(1000, ?MODULE, update_data, [Opts]) of
        {ok, TRef} ->
            {ok, #kinetic_config{tref = TRef}};
        Error ->
            {stop, Error}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Arg, State) ->
    {noreply, State}.

terminate(_Reason, _State = #kinetic_config{tref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    true = ets:delete(?KINETIC_DATA),
    true = ets:delete(?KINETIC_STREAM),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) ->
    error_logger:info_msg("~p: ~p exited due to: ~p~n", [?MODULE, From, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% Internal implementation

region("us-east-1" ++ _R) ->
    "us-east-1";
region("us-west-1" ++ _R) ->
    "us-west-1";
region("us-west-2" ++ _R) ->
    "us-west-2";
region("ap-northeast-1" ++ _R) ->
    "ap-northeast-1";
region("ap-southeast-1" ++ _R) ->
    "ap-southeast-1";
region("eu-west-1" ++ _R) ->
    "eu-west-1".

new_args(Opts) ->
    Region =
        case proplists:get_value(region, Opts, undefined) of
            undefined ->
                {ok, Zone} = imds:zone(),
                region(Zone);
            R ->
                R
        end,

    LHttpcOpts = proplists:get_value(lhttpc_opts, Opts, []),
    DefaultTimeout = proplists:get_value(timeout, Opts, 5000),
    Host = kinetic_utils:endpoint("kinesis", Region),
    Url = "https://" ++ Host,

    %% erliam should support named profiles for using specific roles or preconfigured
    %% long-term credentials to mint session tokens, but for now set keys in erliam app
    %% env if set in kinetic app env; these will be used to create session tokens:
    case {proplists:get_value(aws_access_key_id, Opts),
          proplists:get_value(aws_secret_access_key, Opts)}
    of
        {undefined, _} ->
            ok;
        {_, undefined} ->
            ok;
        {ConfiguredAccessKeyId, ConfiguredSecretAccessKey} ->
            ok = application:set_env(erliam, aws_access_key, ConfiguredAccessKeyId),
            ok = application:set_env(erliam, aws_secret_key, ConfiguredSecretAccessKey),
            ok = erliam:invalidate()
    end,

    #kinetic_arguments{region = Region,
                       date = awsv4:isonow(),
                       host = Host,
                       url = Url,
                       lhttpc_opts = LHttpcOpts,
                       timeout = DefaultTimeout,
                       aws_credentials = erliam:credentials()}.

%% @todo:
%% - rewrite new_args to use this
%% - handle additional args
merge_args(Args, []) ->
    Args;
merge_args(Args, [{region, Region} | Rest]) ->
    Host = kinetic_utils:endpoint("kinesis", Region),
    Url = "https://" ++ Host,
    merge_args(Args#kinetic_arguments{region = Region,
                                      host = Host,
                                      url = Url},
               Rest);
merge_args(Args, [{timeout, Timeout} | Rest]) ->
    merge_args(Args#kinetic_arguments{timeout = Timeout}, Rest).
