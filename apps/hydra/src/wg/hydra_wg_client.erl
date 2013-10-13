-module(hydra_wg_client).

-include("hydra.hrl").
-include("hydra_wg.hrl").

-export([request/1]).



%% Interface



request(Req) when is_record(Req, info_req) ->
    #info_req{
        application_id = AppId,
        ids = Ids
    } = Req,
    Params = [
        {?PARAM_APPID, AppId},
        {?PARAM_IDS, Ids},
        {?PARAM_FIELDS, <<"statistics,account_id,created_at,updated_at,nickname">>}
    ],
    try do_request(?API_POINT_INFO, Params) of {ok, Ret} ->
        {ok, Ret}
    catch throw:Error ->
        {error, Error}
    end;

request(Req) when is_record(Req, tank_req) ->
    #tank_req{
        application_id = AppId,
        ids = Ids
    } = Req,
    Params = [
        {?PARAM_APPID, AppId},
        {?PARAM_IDS, Ids},
        {?PARAM_FIELDS, <<"statistics,tank_id">>}
    ],
    try do_request(?API_POINT_TANK, Params) of {ok, Ret} ->
        {ok, Ret}
    catch throw:Error ->
        {error, Error}
    end.



%% Internals



do_request(Point, Params) ->
    ParamsQ = build_request(Params),
    RequestURI = <<?API_POINT/binary, Point/binary, "?", ParamsQ/binary>>,
    do_http_request(RequestURI).



do_http_request(RequestURI) ->
    case httpc:request(binary_to_list(RequestURI)) of
        {ok, {{_Version, 200, _Phrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, Code, _Phrase}, _Headers, _Body} = Ret}  ->
            ?ERROR("HTTP request (~p) returned ~p code. Ret: ~p", [RequestURI, Code, Ret]),
            throw({invalid_http_response_code, Code});
        {error, Reason} ->
            ?ERROR("HTTP request (~p) returned an error: ~p", [RequestURI, Reason]),
            throw(Reason)
    end.



build_request(Params) ->
    build_request(Params, []).

build_request([{Key, Value} | Params], Acc) when is_list(Value) ->
    build_request(Params, Acc ++ [{Key, join_list(Value)}]);

build_request([{Key, Value} | Params], Acc) when is_binary(Value) ->
    build_request(Params, Acc ++ [{Key, Value}]);

build_request([{Key, Value} | Params], Acc) when is_list(Value) ->
    build_request(Params, Acc ++ [{Key, list_to_binary(Value)}]);

build_request([{Key, Value} | Params], Acc) when is_integer(Value) ->
    build_request(Params, Acc ++ [{Key, ?INT_TO_BIN(Value)}]);

build_request([Param | _Params], _Acc) ->
    throw({unsupported_param_type, Param});

build_request([], Acc) ->
    build_request1(Acc).



build_request1(Params) ->
    build_request1(Params, <<>>).

build_request1([{Key, Value} | Params], <<>>) ->
    build_request1(Params, <<Key/binary, "=", Value/binary>>);

build_request1([{Key, Value} | Params], Acc) ->
    build_request1(Params, <<Acc/binary, "&", Key/binary, "=", Value/binary>>);

build_request1([], Acc) ->
    Acc.



join_list(List) ->
    join_list(List, []).

join_list([Value | List], Acc) when is_binary(Value) ->
    join_list(List, Acc ++ [Value]);

join_list([Value | List], Acc) when is_list(Value) ->
    join_list(List, Acc ++ [list_to_binary(Value)]);

join_list([Value | List], Acc) when is_integer(Value) ->
    join_list(List, Acc ++ [?INT_TO_BIN(Value)]);

join_list([Value | _List], _Acc) ->
    throw({unsupported_paramlist_type, Value});

join_list([], Acc) ->
    join_list1(Acc).



join_list1(List) ->
    join_list1(List, <<>>).

join_list1([Value | List], <<>>) ->
    join_list1(List, Value);

join_list1([Value | List], Acc) ->
    join_list1(List, <<Acc/binary, ",", Value/binary>>);

join_list1([], Acc) ->
    Acc.