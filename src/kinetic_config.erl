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
    MetaData = proplists:get_value(metadata_base_url, Opts, "http://169.254.169.254"),
    Role = proplists:get_value(iam_role, Opts),
    {ok, Zone} = kinetic_utils:fetch_and_return_url(MetaData ++ "/latest/meta-data/placement/availability-zone", text),
    UTCNow = erlang:universaltime(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(UTCNow),

    {CurrentAccessKeyId, CurrentSecretAccessKey, Region, _Date, CurrentExpirationSeconds, LHttpcOpts} =
      case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            % dummy replacements
            {"123", "123", region(Zone), undefined, NowSeconds, proplists:get_value(lhttpc_opts, Opts, [])};

        Result ->
            Result
    end,

    NewDate = kinetic_iso8601:format(UTCNow),
    SecondsToExpire = CurrentExpirationSeconds - NowSeconds,

    NewArgs = case SecondsToExpire < 120 of
        true ->
            {ok, {AccessKeyId, SecretAccessKey, Expiration}} = kinetic_iam:get_aws_keys(MetaData, Role),
            ExpirationSeconds = calendar:datetime_to_gregorian_seconds(kinetic_iso8601:parse(Expiration)),
            {AccessKeyId, SecretAccessKey, Region, NewDate, ExpirationSeconds, LHttpcOpts};

        false ->
            {CurrentAccessKeyId, CurrentSecretAccessKey, Region, NewDate, CurrentExpirationSeconds, LHttpcOpts}
    end,
    ets:insert(?KINETIC_DATA, {?KINETIC_ARGS_KEY, NewArgs}),
    {ok, NewArgs}.


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

