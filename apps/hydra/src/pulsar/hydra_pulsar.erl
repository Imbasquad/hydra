-module(hydra_pulsar).

-behaviour(gen_server).

-include("hydra.hrl").
-include("hydra_metrics.hrl").
-include("hydra_pulsar.hrl").

%% API
-export([start_link/0, set_rps/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    rps :: non_neg_integer(),
    tref :: timer:tref()
}).



%% Interface


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    RPS = ?DEFAULT_RPS,
    {ok, TRef} = init_pulsar(),
    {ok, #state{rps = RPS, tref = TRef}}.



set_rps(RPS) when is_integer(RPS), RPS > 0 ->
    gen_server:call(?MODULE, ?SET_RPS(RPS)).



handle_call(Request, _From, State) ->
    ?WARNING("Invalid call arrived: ~p", [Request]),
    {reply, not_implemented, State}.



handle_cast(Msg, State) ->
    ?WARNING("Invalid cast arrived: ~p", [Msg]),
    {noreply, State}.



handle_info(?SET_RPS(RPS), State) ->
    {noreply, State#state{rps = RPS}};

handle_info(?IMPULSE, State) ->
    handle_impulse(State#state.rps),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("Invalid info arrived: ~p", [Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internals



handle_impulse(RPS) ->
    case hydra_queue:pull(RPS) of
        {error, empty} ->
            nop;
        {ok, ReqList} ->
            metric:inc([?METRIC_CNT_REQ_OUT_TOTAL], length(ReqList)),
            lists:foreach(fun({Delay, Req}) ->
                ?INFO("Running request after ~p", [Delay]),
                timer:apply_after(Delay, hydra_pulsar_worker, req, [Req])
            end, lists:zip(get_delays(RPS, length(ReqList)), ReqList))
    end.



get_delays(RPS, Size) ->
    get_delays(RPS, Size, 0, erlang:round(1000 / RPS) , []).

get_delays(RPS, Size, Delay, Quant, Acc) when length(Acc) < Size ->
    get_delays(RPS, Size, Delay + Quant, Quant, Acc ++ [Delay]);

get_delays(_RPS, _Size, _Delay, _Quant, Acc) ->
    Acc.



init_pulsar() ->
    timer:send_interval(1000, self(), ?IMPULSE).