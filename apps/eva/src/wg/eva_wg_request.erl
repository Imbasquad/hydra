-module(eva_wg_request).

-include("papi.hrl").

-export([info/1, tank/1]).



%% Interface



info(Ids) when is_list(Ids) ->
    Req = #info_req{
        application_id = eva_env:get(application_id, <<"DEFAULT_APPLICATION_ID">>),
        ids = Ids
    },
    eva_queue:push(3, Req).

tank(Ids) when is_list(Ids) ->
    Req = #tank_req{
        application_id = eva_env:get(application_id, <<"DEFAULT_APPLICATION_ID">>),
        ids = Ids
    },
    eva_queue:push(3, Req).



