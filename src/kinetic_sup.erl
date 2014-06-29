-module(kinetic_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

-export([update_data/1, setup/1]).

-include("kinetic.hrl").

-type child() :: {atom(), {atom(), atom(), list(any)},
    atom(), integer(), atom(), list(atom())}.

-spec start_link() -> {ok, pid()} | {error, atom()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(any()) -> {ok, {{atom(), integer(), integer()}, [child()]}}.
init(Opts) ->
    setup(Opts),
    {ok, {{simple_one_for_one, 10, 1}, [
        {kinetic_logger,
         {kinetic_logger, start_link, []},
         transient, 10000, worker, [kinetic_logger]}
    ]}}.

setup(Opts) ->
    ets:new(?KINETIC_DATA, [named_table, set, public, {read_concurrency, true}]),
    case update_data(Opts) of
        {ok, ClientArgs} ->
            case timer:apply_interval(1000, ?MODULE, update_data, [Opts]) of
                {ok, _TRef} -> {ok, ClientArgs};
                Error -> Error
            end;
        Error -> Error
    end.

% -spec region(zone()) -> region().
region("us-east-1" ++ _R) -> "us-east-1";
region("us-west-1" ++ _R) -> "us-west-1";
region("us-west-2" ++ _R) -> "us-west-2";
region("ap-northeast-1" ++ _R) -> "ap-northeast-1";
region("ap-southeast-1" ++ _R) -> "ap-southeast-1";
region("eu-west-1" ++ _R) -> "eu-west-1".

update_data(Opts) ->
    MetaData = proplists:get_value(metadata_base_url, Opts, "http://169.254.169.254"),
    Role = proplists:get_value(iam_role, Opts),
    {ok, Zone} = kinetic_utils:fetch_and_return_url(MetaData ++ "/latest/meta-data/placement/availability-zone", text),
    UTCNow = erlang:universaltime(),

    {CurrentAccessKeyId,
     CurrentSecretAccessKey,
     Region,
     _Date,
     CurrentExpirationSeconds,
     LHttpcOpts} = case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {"123",
             "123",
             region(Zone),
             undefined,
             calendar:datetime_to_gregorian_seconds(UTCNow),
             proplists:get_value(lhttpc_opts, Opts, [])};

        Result ->
            Result
    end,

    NewDate = kinetic_iso8601:format(UTCNow),
    NowSeconds = calendar:datetime_to_gregorian_seconds(UTCNow),
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


