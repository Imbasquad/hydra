-module(eva_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eva_sup:start_link().

stop(_State) ->
    ok.
