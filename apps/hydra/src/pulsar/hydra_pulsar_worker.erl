-module(hydra_pulsar_worker).

-behaviour(gen_server).

-include("hydra.hrl").
-include("hydra_queue.hrl").
-include("hydra_pulsar_worker.hrl").

%% API
-export([start_link/0, start/0, stop/1, req/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).



%% Interface



-spec start() ->
    {ok, Child :: pid()} | 
    {ok, Child :: pid(), Info :: term()} | 
    {error, Reason :: already_present} |
    {error, Reason :: {already_started, Child :: pid()}} |
    {error, Reason :: term()}.
start() ->
    supervisor:start_child(hydra_pulsar_worker_sup, []).



-spec stop(Pid :: pid()) ->
    ok | {error, Reason :: term()}.
stop(Pid) ->
    supervisor:terminate_child(hydra_pulsar_worker_sup, Pid).



-spec req(Req :: #hydra_queue_req{}) ->
    ok.
req(Req) when is_record(Req, hydra_queue_req) ->
    {ok, Pid} = start(),
    gen_server:cast(Pid, ?REQ(Req)),
    ok.



-spec start_link() ->
    {ok, Pid :: pid()} |
    ignore |
    {error, Error :: any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).



-spec init(Args :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} |
    ignore.
init([]) ->
    {ok, #state{}}.



-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(?REQ(Req), State) ->
    #hydra_queue_req{
        from = {Pid, _Tag} = From,
        uri = URI
    } = Req,
    case erlang:process_info(Pid, status) of
        {status, waiting} ->
            Ret = hydra_http:req(URI),
            gen_server:reply(From, Ret);
        {status, Status} ->
            ?WARNING("Caller process is in status '~p', dropping the task", [Status]);
        Other ->
            ?WARNING("Caller process status call returned '~p', dropping the task", [Other])
    end,
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
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



