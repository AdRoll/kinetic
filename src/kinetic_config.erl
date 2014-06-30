-module(kinetic_config).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([start_link/1, update_data/1, stop/0, g/1]).

-include("kinetic.hrl").

-record(kinetic_config, {tref}).

start_link(Opts) ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, [Opts], []).
stop() ->
    gen_server:cast(?MODULE, stop).
g(Name) ->
    case application:get_env(kinetic, Name) of
        {ok, {local, PathElems}} ->
            dyno_deps:local_path(PathElems);
        {ok, {abs, Path}} ->
            Path;
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.
update_data(Opts) ->
    Arguments = case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            update_data_first(Opts);
        Result ->
            update_data_subsequent(Opts, Result)
    end,
    ets:insert(?KINETIC_DATA, {?KINETIC_ARGS_KEY, Arguments}),
    {ok, Arguments}.

% gen_server behavior

init([Opts]) ->
    ets:new(?KINETIC_DATA, [named_table, set, public, {read_concurrency, true}]),
    case update_data(Opts) of
        {ok, _ClientArgs} ->
            case timer:apply_interval(1000, ?MODULE, update_data, [Opts]) of
                {ok, TRef} -> 
                    {ok, #kinetic_config{tref=TRef}};
                Error ->
                    {error, Error}
            end;
        Error -> 
            {error, Error}
    end.

handle_call(_Arg, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Arg, State) ->
    {noreply, State}.

terminate(_Reason, _State=#kinetic_config{tref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    true = ets:delete(?KINETIC_DATA),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

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

update_data_first(Opts) ->
    ConfiguredAccessKeyId = proplists:get_value(aws_access_key_id, Opts),
    ConfiguredSecretAccessKey = proplists:get_value(aws_secret_access_key, Opts),
    LHttpcOpts = proplists:get_value(lhttpc_opts, Opts, []),

    case {ConfiguredAccessKeyId, ConfiguredSecretAccessKey} of
        {V, P} when V =:= undefined orelse P =:= undefined ->
            % setup from IAM service
            refresh_from_iam(Opts);
        _ ->
            % These keys never expire
            MetaData = proplists:get_value(metadata_base_url, Opts, "http://169.254.169.254"),
            {ok, Zone} = kinetic_utils:fetch_and_return_url(MetaData ++ "/latest/meta-data/placement/availability-zone", text),
            Region = region(Zone),
            #kinetic_arguments{access_key_id=ConfiguredAccessKeyId,
                               secret_access_key=ConfiguredSecretAccessKey,
                               region=Region,
                               date=isonow(),
                               expiration_seconds=undefined,
                               lhttpc_opts=LHttpcOpts}
    end.

update_data_subsequent(_Opts, Args=#kinetic_arguments{expiration_seconds=undefined}) ->
    Args#kinetic_arguments{date=isonow()};
update_data_subsequent(Opts, Args=#kinetic_arguments{expiration_seconds=CurrentExpirationSeconds}) ->
    SecondsToExpire = CurrentExpirationSeconds - calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    case SecondsToExpire < 120 of
        true ->
            refresh_from_iam(Opts);
        false ->
            Args#kinetic_arguments{date=isonow()}
    end.

refresh_from_iam(Opts) ->
    MetaData = proplists:get_value(metadata_base_url, Opts, "http://169.254.169.254"),
    {ok, Zone} = kinetic_utils:fetch_and_return_url(MetaData ++ "/latest/meta-data/placement/availability-zone", text),
    Region = region(Zone),
    LHttpcOpts = proplists:get_value(lhttpc_opts, Opts, []),
    Role = proplists:get_value(iam_role, Opts),

    {ok, {AccessKeyId, SecretAccessKey, Expiration}} = kinetic_iam:get_aws_keys(MetaData, Role),
    ExpirationSeconds = calendar:datetime_to_gregorian_seconds(kinetic_iso8601:parse(Expiration)),
    #kinetic_arguments{access_key_id=AccessKeyId,
                       secret_access_key=SecretAccessKey,
                       region=Region,
                       date=isonow(),
                       expiration_seconds=ExpirationSeconds,
                       lhttpc_opts=LHttpcOpts}.


isonow() ->
    kinetic_iso8601:format(erlang:universaltime()).

