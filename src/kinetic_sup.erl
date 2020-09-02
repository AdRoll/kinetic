-module(kinetic_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1, stop/1]).

-include("kinetic.hrl").

start_link() ->
    start_link([]).

% Need the slightly stupid double code here to avoid an infinite loop
% in case kinetic_config:g(args) -> []
start_link([]) ->
    Args = kinetic_config:g(args),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args);
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Opts) ->
    KineticConfig =
        {kinetic_config,
         {kinetic_config, start_link, [Opts]},
         permanent,
         10000,
         worker,
         [kinetic_config]},

    KineticStreamSup =
        {kinetic_stream_sup,
         {kinetic_stream_sup, start_link, []},
         permanent,
         10000,
         supervisor,
         dynamic},

    {ok, {{one_for_one, 10, 1}, [KineticConfig, KineticStreamSup]}}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, _, _} ->
            ok
    end.
