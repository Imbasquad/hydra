-module(hydra_queue_ets).

-include("hydra.hrl").
-include("hydra_queue_ets.hrl").

-export([new/1, pull/1, push/3]).

-export_type([queue_ets/0]).



%% Types



-type queue_ets() :: {hydra_queue_ets, atom(), hydra_queue_bag:bag()}.



%% Interface


-spec new(list()) -> hydra_queue_ets:queue_ets().
new(Tables) when is_list(Tables), length(Tables) == 5 ->
    #hydra_queue_ets{
        tables = Tables,
        bag = hydra_queue_bag:new()
    }.



-spec pull(hydra_queue_ets:queue_ets()) -> {error, empty} | {ok, hydra_queue_ets:queue_ets(), term()}.
pull(Queue) when is_record(Queue, hydra_queue_ets) ->
    #hydra_queue_ets{
        tables = Tables,
        bag = Bag
    } = Queue,
    Available = hydra_queue_bag:available(Bag),
    case Available of
        [] ->
            pull(Queue#hydra_queue_ets{bag = hydra_queue_bag:reset(Bag)});
        List ->
            case pull(Tables, List, Bag) of
                {error, empty} when length(Available) == 5 ->
                    {error, empty};
                {error, empty} ->
                    pull(Queue#hydra_queue_ets{bag = hydra_queue_bag:reset(Bag)});
                {ok, Bag1, Ret} ->
                    {ok, Queue#hydra_queue_ets{bag = Bag1}, Ret}
            end
    end.

-spec pull(list(), list(), hydra_queue_bag:bag()) -> {error, empty} | {ok, hydra_queue_ets:queue_ets(), term()}.
pull(_Tables, [], _Bag) ->
    {error, empty};

pull(Tables, [P1 | P], Bag) ->
    Table = lists:nth(P1, Tables),
    case ets:first(Table) of
        '$end_of_table' ->
            pull(Tables, P, Bag);
        Key ->
            {ok, Bag1} = hydra_queue_bag:buy(Bag, P1),
            [Req] = ets:lookup(Table, Key),
            ets:delete(Table, Key),
            {ok, Bag1, Req}
    end.


-spec push(hydra_queue_ets:queue_ets(), non_neg_integer(), term()) -> {ok, pushed}.
push(Queue, Priority, Req) ->
    Table = lists:nth(Priority, Queue#hydra_queue_ets.tables),
    ets:insert(Table, Req),
    {ok, pushed}.
