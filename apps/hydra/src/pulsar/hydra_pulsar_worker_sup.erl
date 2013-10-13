-module(hydra_pulsar_worker_sup).

-behaviour(supervisor).

-include("supervisor.hrl").

-export([start_link/0]).
-export([init/1]).



%% Callbacks



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        ?CHILD(hydra_pulsar_worker, [], temporary, brutal_kill, worker)
    ]}}.



%% Internals


