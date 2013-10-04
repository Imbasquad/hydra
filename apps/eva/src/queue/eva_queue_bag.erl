-module(eva_queue_bag).

-include("eva.hrl").
-include("eva_queue_bag.hrl").

-export([new/0, new_linear/0, available/1, buy/2, reset/1]).

-export_type([bag/0]).



%% Types



-type bag() :: {eva_queue_bag, list(), list(), list(), non_neg_integer()}.



%% Interface


-spec new() -> eva_queue_bag:bag().
new() ->
    new_linear().



-spec new_linear() -> eva_queue_bag:bag().
new_linear() ->
    #eva_queue_bag{
        p = ps(),
        pb = pbags(),
        pl = plimits_linear(),
        prc = 1
    }.


-spec available(eva_queue_bag:bag()) -> list().
available(#eva_queue_bag{p = P, pb = PB, pl = PL}) ->
    available(P, [], PB, PL, [], []).

-spec available(list(), list(), list(), list(), list(), list()) -> list().
available([], _PAcc, [], [], _PLAcc, Acc) ->
    Acc;

available([P1 | []], PAcc, [PB1 | []], [PL1 | []], PLAcc, []) when PB1 == PL1 ->
    available(PAcc ++ [P1], [], pbags(), PLAcc ++ [PL1], [], []);

available([P1 | P], PAcc, [PB1 | PB], [PL1 | PL], PLAcc, Acc) when PB1 == PL1 ->
    available(P, PAcc ++ [P1], PB, PL, PLAcc ++ [PL1], Acc);

available([P1 | P], PAcc, [PB1 | PB], [PL1 | PL], PLAcc, Acc) when PB1 =< PL1 ->
    available(P, PAcc ++ [P1], PB, PL, PLAcc ++ [P1], Acc ++ [P1]).



-spec buy(eva_queue_bag:bag(), non_neg_integer()) -> {ok, eva_queue_bag:bag()}.
buy(Bag, P) when is_record(Bag, eva_queue_bag) ->
    #eva_queue_bag{pb = PB0, prc = Prc} = Bag,
    PB = lists:nth(P, PB0),
    PB1 = lists:sublist(PB0, 1, P - 1) ++ [PB + Prc] ++ lists:nthtail(P, PB0),
    {ok, Bag#eva_queue_bag{pb = PB1}}.



-spec reset(eva_queue_bag:bag()) -> eva_queue_bag:bag().
reset(Bag) when is_record(Bag, eva_queue_bag) ->
    Bag#eva_queue_bag{pb = pbags()}.



%% Internals



ps() ->
    lists:seq(1, 5).



pbags() ->
    lists:foldl(fun(_, Acc) ->
        Acc ++ [0]
    end, [], lists:seq(1,5)).



plimits_linear() ->
    lists:foldl(fun(P, Acc) ->
        [P | Acc]
    end, [], lists:seq(1,5)).