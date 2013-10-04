-module(eva_queue_head).

-behaviour(gen_server).

-include("eva.hrl").
-include("eva_queue.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    queue_ets :: eva_queue_ets:queue_ets()
}).



%% Interface



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    {ok, Queue} = gen_server:call(eva_queue_tail, ?QUEUE_TRANSFER),
    ?INFO("Queue transfer complete"),
    {ok, #state{queue_ets = Queue}}.



%% Callbacks



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



handle_cast(Msg, State) ->
    ?WARNING("Invalid cast arrived: ~p", [Msg]),
    {noreply, State}.



handle_info(Info, State) ->
    ?WARNING("Invalid info arrived: ~p", [Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internals



do_pull(Queue, Count) ->
    do_pull(Queue, Count, []).

do_pull(_Queue, 0, []) ->
    {error, empty};

do_pull(Queue, 0, Acc) ->
    {ok, Queue, Acc};

do_pull(Queue, Count, Acc) ->
    case eva_queue_ets:pull(Queue) of
        {error, empty} ->
            do_pull(Queue, 0, Acc);
        {ok, Queue1, Ret} ->
            do_pull(Queue1, Count - 1, Acc ++ [Ret])
    end.
