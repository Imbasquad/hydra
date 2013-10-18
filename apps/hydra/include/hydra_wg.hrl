-define(API_POINT, <<"http://api.worldoftanks.ru/wot">>).
-define(PARAM_APPID, <<"application_id">>).
-define(PARAM_IDS, <<"account_id">>).
-define(PARAM_FIELDS, <<"fields">>).



-define(API_POINT_INFO, <<"/account/info/">>).
-record(info_req, {
    application_id :: binary(),
    ids :: list()
}).



-define(API_POINT_TANK, <<"/account/tanks/">>).
-record(tank_req, {
    application_id :: binary(),
    ids :: list()
}).