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



start() ->
    supervisor:start_child(hydra_pulsar_worker_sup, []).



stop(Pid) ->
    supervisor:terminate_child(hydra_pulsar_worker_sup, Pid).



req(Req) when is_record(Req, hydra_queue_req) ->
    {ok, Pid} = start(),
    gen_server:cast(Pid, ?REQ(Req)),
    ok.



%% Callbacks



start_link() ->
    gen_server:start_link(?MODULE, [], []).



init([]) ->
    {ok, #state{}}.



handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



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



handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internals



