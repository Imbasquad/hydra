-module(hydra_queue).

-include("hydra.hrl").
-include("hydra_queue.hrl").

-export([push/2, pull/1]).



%% Interface


-spec push(non_neg_integer(), term()) -> {ok, term()}.
push(Priority, Payload)
when is_integer(Priority), Priority < 6, Priority > 0 ->
    gen_server:call(hydra_queue_tail, ?PUSH_CMD(Priority, Payload), 60000).


-spec pull(non_neg_integer()) -> {error, empty} | {ok, list()}.
pull(Count) when is_integer(Count) ->
    gen_server:call(hydra_queue_head, ?PULL_CMD(Count), 60000).
