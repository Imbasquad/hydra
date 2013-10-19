-module(hydra_pulsar).

-behaviour(gen_server).

-include("hydra.hrl").
-include("hydra_metrics.hrl").
-include("hydra_pulsar.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    rps :: non_neg_integer(),
    tref :: timer:tref()
}).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    RPS = hydra_env:get(requests_per_second, 15),
    {ok, TRef} = init_pulsar(),
    {ok, #state{rps = RPS, tref = TRef}}.



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(?IMPULSE, State) ->
    handle_impulse(State#state.rps),
    {noreply, State};

handle_info(_Info, State) ->
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
            start_workers(ReqList)
    end.



start_workers([]) ->
    ok;

start_workers([Req | ReqList]) ->
    hydra_pulsar_worker:req(Req),
    start_workers(ReqList).



init_pulsar() ->
    timer:send_interval(1000, self(), ?IMPULSE).