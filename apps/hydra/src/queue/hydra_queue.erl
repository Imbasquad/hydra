-module(hydra_queue).

-include("hydra.hrl").
-include("hydra_queue.hrl").

-export([push/2, pull/1]).



-type queue_req_priority() :: 1 | 2 | 3 | 4 | 5.
-type queue_req_uri() :: string().
-type queue_req_ret() :: binary().
-type queue_req() :: #hydra_queue_req{}.

-export_type([queue_req_priority/0, queue_req_uri/0, queue_req_ret/0, queue_req/0]).



%% Interface



-spec push(queue_req_priority(), queue_req_uri()) ->
    {ok, queue_req_ret()}.
push(Priority, Payload)
when is_integer(Priority), Priority < 6, Priority > 0 ->
    gen_server:call(hydra_queue_tail, ?PUSH_CMD(Priority, Payload), 60000).



-spec pull(non_neg_integer()) ->
    {error, empty} | {ok, [#hydra_queue_req{}]}.
pull(Count) when is_integer(Count) ->
    gen_server:call(hydra_queue_head, ?PULL_CMD(Count), 60000).
