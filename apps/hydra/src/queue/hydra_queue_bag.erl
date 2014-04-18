-module(hydra_queue_bag).

-include("hydra.hrl").
-include("hydra_queue_bag.hrl").

-export([new/0, new_linear/0, available/1, buy/2, reset/1]).



-type bag() :: #hydra_queue_bag{}.

-export_type([bag/0]).



%% Interface



-spec new() ->
    hydra_queue_bag:bag().
new() ->
    new_linear().



-spec new_linear() ->
    hydra_queue_bag:bag().
new_linear() ->
    #hydra_queue_bag{
        p = ps(),
        pb = pbags(),
        pl = plimits_linear(),
        prc = 1
    }.



-spec available(hydra_queue_bag:bag()) ->
    list().
available(#hydra_queue_bag{p = P, pb = PB, pl = PL}) ->
    available(P, [], PB, PL, [], []).



-spec available([non_neg_integer()], [non_neg_integer()], [non_neg_integer()], [non_neg_integer()], [non_neg_integer()], [non_neg_integer()]) ->
    [hydra_queue:queue_req_priority()].
available([], _PAcc, [], [], _PLAcc, Acc) ->
    Acc;

available([P1 | []], PAcc, [PB1 | []], [PL1 | []], PLAcc, []) when PB1 == PL1 ->
    available(PAcc ++ [P1], [], pbags(), PLAcc ++ [PL1], [], []);

available([P1 | P], PAcc, [PB1 | PB], [PL1 | PL], PLAcc, Acc) when PB1 == PL1 ->
    available(P, PAcc ++ [P1], PB, PL, PLAcc ++ [PL1], Acc);

available([P1 | P], PAcc, [PB1 | PB], [PL1 | PL], PLAcc, Acc) when PB1 =< PL1 ->
    available(P, PAcc ++ [P1], PB, PL, PLAcc ++ [P1], Acc ++ [P1]).



-spec buy(Bag :: hydra_queue_bag:bag(), P :: hydra_queue:queue_req_priority()) ->
    {ok, hydra_queue_bag:bag()}.
buy(Bag, P) when is_record(Bag, hydra_queue_bag) ->
    #hydra_queue_bag{pb = PB0, prc = Prc} = Bag,
    PB = lists:nth(P, PB0),
    PB1 = lists:sublist(PB0, 1, P - 1) ++ [PB + Prc] ++ lists:nthtail(P, PB0),
    {ok, Bag#hydra_queue_bag{pb = PB1}}.



-spec reset(Bag :: hydra_queue_bag:bag()) ->
    hydra_queue_bag:bag().
reset(Bag) when is_record(Bag, hydra_queue_bag) ->
    Bag#hydra_queue_bag{pb = pbags()}.



%% Internals



-spec ps() ->
    [hydra_queue:queue_req_priority()].
ps() ->
    lists:seq(1, 5).



-spec pbags() ->
    [non_neg_integer()].
pbags() ->
    [0 || _ <- lists:seq(1, 5)].



-spec plimits_linear() ->
    [non_neg_integer()].
plimits_linear() ->
    [P || P <- lists:reverse(lists:seq(1, 5))].