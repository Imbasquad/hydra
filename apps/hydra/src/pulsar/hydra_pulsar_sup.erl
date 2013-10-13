-module(hydra_pulsar_sup).

-behaviour(supervisor).

-include("supervisor.hrl").

-export([start_link/0]).
-export([init/1]).



%% Callbacks



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 0, 1}, [
        ?GENERIC_WORKER(hydra_pulsar),
        ?GENERIC_SUPERVISOR(hydra_pulsar_worker_sup)
    ]}}.



%% Internals


