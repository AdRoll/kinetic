-module(kinetic_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

-include("kinetic.hrl").

-type child() :: {atom(), {atom(), atom(), list(any)},
    atom(), integer(), atom(), list(atom())}.

-spec start_link() -> {ok, pid()} | {error, atom()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(any()) -> {ok, {{atom(), integer(), integer()}, [child()]}}.
init(_Opts) ->
    init_ets(),
    {ok, {{simple_one_for_one, 10, 1}, [
        {kinetic_logger,
         {kinetic_logger, start_link, []},
         transient, 10000, worker, [kinetic_logger]}
    ]}}.

init_ets() ->
    ets:new(?KINETIC_DATA, [named_table, set, public, {read_concurrency, true}]).




