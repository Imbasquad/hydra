-module(eva_sup).

-behaviour(supervisor).

-include("eva.hrl").
-include("supervisor.hrl").

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, [[]]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        ?GENERIC_SUPERVISOR(eva_queue_sup),
        ?GENERIC_SUPERVISOR(eva_pulsar_sup)
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
