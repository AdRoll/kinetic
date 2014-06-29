-module(kinetic).
-behaviour(application).


-export([start/0, stop/0]).
-export([start/2, stop/1]).


-spec start() -> ok | {error, any()}.
start() ->
    application:start(lhttpc).


-spec stop() -> ok | {error, any()}.
stop() ->
    application:stop(lhttpc).


-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.
start(_, Opts) when is_list(Opts) ->
    kinetic_sup:start_link(Opts);
start(_, _) ->
    kinetic_sup:start_link().

-spec stop(any()) -> ok.
stop(_) ->
    ok.

