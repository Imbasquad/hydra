-module(eva_queue_ets).

-include("eva.hrl").
-include("eva_queue_ets.hrl").

-export([new/1, pull/1, push/3]).

-export_type([queue_ets/0]).



%% Types



-type queue_ets() :: {eva_queue_ets, atom(), eva_queue_bag:bag()}.



%% Interface


-spec new(list()) -> eva_queue_ets:queue_ets().
new(Tables) when is_list(Tables), length(Tables) == 5 ->
    #eva_queue_ets{
        tables = Tables,
        bag = eva_queue_bag:new()
    }.



-spec pull(eva_queue_ets:queue_ets()) -> {error, empty} | {ok, eva_queue_ets:queue_ets(), term()}.
pull(Queue) when is_record(Queue, eva_queue_ets) ->
    #eva_queue_ets{
        tables = Tables,
        bag = Bag
    } = Queue,
    case eva_queue_bag:available(Bag) of
        [] ->
            pull(Queue#eva_queue_ets{bag = eva_queue_bag:reset(Bag)});
        List ->
            case pull(Tables, List, Bag) of
                {error, empty} ->
                    {error, empty};
                {ok, Bag1, Ret} ->
                    {ok, Queue#eva_queue_ets{bag = Bag1}, Ret}
            end
    end.

-spec pull(list(), list(), eva_queue_bag:bag()) -> {error, empty} | {ok, eva_queue_ets:queue_ets(), term()}.
pull(_Tables, [], _Bag) ->
    {error, empty};

pull(Tables, [P1 | P], Bag) ->
    Table = lists:nth(P1, Tables),
    case ets:first(Table) of
        '$end_of_table' ->
            pull(Tables, P, Bag);
        Key ->
            {ok, Bag1} = eva_queue_bag:buy(Bag, P1),
            {ok, Bag1, ets:lookup(Table, Key)}
    end.


-spec push(eva_queue_ets:queue_ets(), non_neg_integer(), term()) -> {ok, pushed}.
push(Queue, Priority, Req) ->
    Table = lists:nth(Priority, Queue#eva_queue_ets.tables),
    ets:insert(Table, Req),
    {ok, pushed}.
