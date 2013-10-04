-module(eva_queue).

-include("eva.hrl").
-include("eva_queue.hrl").

-export([push/2, pull/1]).



%% Interface


-spec push(non_neg_integer(), term()) -> {ok, term()}.
push(Priority, Payload)
when is_integer(Priority), Priority < 6, Priority > 0 ->
    gen_server:call(eva_queue_tail, ?PUSH_CMD(Priority, Payload), infinity).


-spec pull(non_neg_integer()) -> {error, empty} | {ok, list()}.
pull(Count) when is_integer(Count) ->
    gen_server:call(eva_queue_head, ?PULL_CMD(Count), infinity).
