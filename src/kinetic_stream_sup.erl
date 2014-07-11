-module(kinetic_stream_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("kinetic.hrl").

-type child() :: {atom(), {atom(), atom(), list(any)},
    atom(), integer(), atom(), list(atom())}.

-spec start_link() -> {ok, pid()} | {error, atom()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [[]]).

-spec init(any()) -> {ok, {{atom(), integer(), integer()}, [child()]}}.
init(_) ->
    KineticStream = {kinetic_stream,
                     {kinetic_stream, start_link, []},
                     transient, 10000, worker, [kinetic_stream]},

    {ok, {{simple_one_for_one, 10, 1}, [KineticStream]}}.

