-module(hydra_pulsar).

-behaviour(gen_server).

-include("hydra.hrl").
-include("hydra_metrics.hrl").
-include("hydra_pulsar.hrl").

-export([start_link/0, set_rps/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



-record(state, {
    rps :: non_neg_integer(),
    tref :: timer:tref()
}).



%% Interface



-spec start_link() ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



-spec init(Args :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
init([]) ->
    RPS = ?DEFAULT_RPS,
    {ok, TRef} = init_pulsar(),
    {ok, #state{rps = RPS, tref = TRef}}.



-spec set_rps(RPS :: integer()) ->
    ok.
set_rps(RPS) when is_integer(RPS), RPS > 0 ->
    gen_server:call(?MODULE, ?SET_RPS(RPS)).



-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(?SET_RPS(RPS), _From, State) ->
    {reply, ok, State#state{rps = RPS}};

handle_call(Request, _From, State) ->
    ?WARNING("Invalid call arrived: ~p", [Request]),
    {reply, not_implemented, State}.



-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(Msg, State) ->
    ?WARNING("Invalid cast arrived: ~p", [Msg]),
    {noreply, State}.



-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(?IMPULSE, State) ->
    handle_impulse(State#state.rps),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("Invalid info arrived: ~p", [Info]),
    {noreply, State}.



-spec terminate(Reason :: any(), State :: #state{}) ->
    ok.
terminate(_Reason, _State) ->
    ok.



-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) ->
    {ok, State :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internals



-spec handle_impulse(RPS :: non_neg_integer()) ->
    ok.
handle_impulse(RPS) ->
    case hydra_queue:pull(RPS) of
        {error, empty} ->
            ok;
        {ok, ReqList} ->
            metric:inc([?METRIC_CNT_REQ_OUT_TOTAL], length(ReqList)),
            lists:foreach(fun({Delay, Req}) ->
                ?INFO("Running request after ~p", [Delay]),
                timer:apply_after(Delay, hydra_pulsar_worker, req, [Req])
            end, lists:zip(get_delays(RPS, length(ReqList)), ReqList)),
            ok
    end.



-spec get_delays(RPS :: non_neg_integer(), Size :: non_neg_integer()) ->
    [non_neg_integer()].
get_delays(RPS, Size) ->
    get_delays(RPS, Size, 0, erlang:round(1000 / RPS) , []).



-spec get_delays(
    RPS :: non_neg_integer(),
    Size :: non_neg_integer(),
    Delay :: non_neg_integer(),
    Quant :: non_neg_integer(),
    Acc :: [non_neg_integer()]
) ->
    [non_neg_integer()].
get_delays(RPS, Size, Delay, Quant, Acc) when length(Acc) < Size ->
    get_delays(RPS, Size, Delay + Quant, Quant, Acc ++ [Delay]);

get_delays(_RPS, _Size, _Delay, _Quant, Acc) ->
    Acc.



-spec init_pulsar() ->
    {ok, TRef :: timer:tref()}.
init_pulsar() ->
    timer:send_interval(1000, self(), ?IMPULSE).