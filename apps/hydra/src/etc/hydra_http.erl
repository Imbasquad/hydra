-module(hydra_http).

-export([req/1]).



%% Interface



-spec req(Req :: hydra_queue:queue_req_uri()) ->
    {ok, Body :: binary()} |
    {error, Status :: non_neg_integer()} |
    {error, Reason :: term()}.
req(Req) ->
    case httpc:request(Req) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, Status, _}, _Headers, _Body}} ->
            {error, Status};
        {error, Reason} ->
            {error, Reason}
    end.
