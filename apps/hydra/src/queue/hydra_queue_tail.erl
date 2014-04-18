-module(hydra_queue_tail).

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
    Tables = lists:foldl(fun(Priority, Acc) ->
        Table = ?TABLE_NAME_BY_PRIORITY(Priority),
        Table = ets:new(Table, [ordered_set, named_table, public, {keypos, 2}]),
        Acc ++ [Table]
    end, [], lists:seq(1,5)),
    Queue = hydra_queue_ets:new(Tables),
    {ok, #state{queue_ets = Queue}}.



%% Callbacks



-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(?QUEUE_TRANSFER, _From, #state{queue_ets = Queue} = State) ->
    ?INFO("Queue transfer"),
    {reply, {ok, Queue}, State};

handle_call(?PUSH_CMD(Priority, Payload), From, #state{queue_ets = Queue} = State) ->
    Req = #hydra_queue_req{
        created_at = erlang:now(),
        from = From,
        uri = Payload
    },
    {ok, pushed} = hydra_queue_ets:push(Queue, Priority, Req),
    {noreply, State};

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




