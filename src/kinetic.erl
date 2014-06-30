-module(kinetic).
-behaviour(application).


-export([start/0, stop/0]).
-export([start/2, stop/1]).

-export([start/1]).
-export([list_streams/1]).


-include("kinetic.hrl").

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


get_args() ->
    case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {error, missing_credentials};
        V ->
            {ok, V}
    end.


% -spec api(method(), any(), integer()) ->result().
list_streams(_Limit) ->
    case get_args() of
        {error, Error} ->
            {error, Error};

        {ok, _V} ->%{ApiAccessKeyId, ApiSecretAccessKey, Region, Date, LHttpcOpts}} ->
            pass
    end.

% api(Name, Body, Timeout) ->
%     case catch(ets:lookup_element(?DINERL_DATA, ?ARGS_KEY, 2)) of
%         {'EXIT', {badarg, _}} ->
%             {error, missing_credentials, ""};
%         {ApiAccessKeyId, ApiSecretAccessKey, Zone, ApiToken, Date, _} ->
%             dinerl_client:api(ApiAccessKeyId, ApiSecretAccessKey, Zone,
%                               ApiToken, Date, Name, Body, Timeout)
%     end.
