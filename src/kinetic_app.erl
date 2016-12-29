-module(kinetic_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, StartArgs) ->
    kinetic_sup:start_link(StartArgs).


stop(_State) ->
    ok.
