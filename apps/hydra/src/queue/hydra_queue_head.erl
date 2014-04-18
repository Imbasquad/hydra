-module(hydra_queue_head).

-behaviour(gen_server).

-include("hydra.hrl").
-include("hydra_queue.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    queue_ets :: hydra_queue_ets:queue_ets()
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
    {ok, Queue} = gen_server:call(hydra_queue_tail, ?QUEUE_TRANSFER),
    ?INFO("Queue transfer complete"),
    {ok, #state{queue_ets = Queue}}.



-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(?PULL_CMD(_Count), _From, #state{queue_ets = undefined} = State) ->
    {reply, {error, not_ready}, State};

handle_call(?PULL_CMD(Count), _From, #state{queue_ets = Queue} = State) ->
    case do_pull(Queue, Count) of
        {error, empty} ->
            {reply, {error, empty}, State};
        {ok, Queue1, Result} ->
            {reply, {ok, Result}, State#state{queue_ets = Queue1}}
    end;

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



-spec do_pull(Queue :: hydra_queue_ets:queue_ets(), Count :: non_neg_integer()) ->
    {error, empty} | {ok, Queue :: hydra_queue_ets:queue_ets(), [hydra_queue:queue_req()]}.
do_pull(Queue, Count) ->
    do_pull(Queue, Count, []).

do_pull(_Queue, 0, []) ->
    {error, empty};

do_pull(Queue, 0, Acc) ->
    {ok, Queue, Acc};

do_pull(Queue, Count, Acc) ->
    case hydra_queue_ets:pull(Queue) of
        {error, empty} ->
            do_pull(Queue, 0, Acc);
        {ok, Queue1, Ret} ->
            do_pull(Queue1, Count - 1, Acc ++ [Ret])
    end.
