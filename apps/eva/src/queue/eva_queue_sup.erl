-module(eva_queue_sup).

-behaviour(supervisor).

-include("eva.hrl").
-include("supervisor.hrl").

-export([start_link/0]).
-export([init/1]).



%% Callbacks



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        ?GENERIC_WORKER(eva_queue_tail),
        ?GENERIC_WORKER(eva_queue_head)
    ],
    {ok, {{one_for_all, 5, 10}, Children}}.



%% Internals


