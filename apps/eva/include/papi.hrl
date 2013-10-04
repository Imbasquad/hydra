-define(API_POINT, <<"http://api.worldoftanks.ru/2.0">>).
-define(PARAM_APPID, <<"application_id">>).
-define(PARAM_IDS, <<"account_id">>).



-define(API_POINT_INFO, <<"/account/info/">>).
-record(info_req, {
    application_id :: binary(),
    ids :: list()
}).



-define(API_POINT_VEHICLE, <<"/account/tanks/">>).
-record(vehicle_req, {
    application_id :: binary(),
    ids :: list()
}).