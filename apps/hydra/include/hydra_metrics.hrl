
%% Counters



-define(METRIC_CNT_REQ_OUT_TOTAL, [count, req, outcoming, total]).
-define(METRIC_CNT_REQ_OUT_INFO, [count, req, outcoming, info]).
-define(METRIC_CNT_REQ_OUT_TANK, [count, req, outcoming, tank]).
-define(METRIC_CNT_REQ_INC_TOTAL, [count, req, incoming, total]).
-define(METRIC_CNT_REQ_INC_INFO, [count, req, incoming, info]).
-define(METRIC_CNT_REQ_INC_TANK, [count, req, incoming, tank]).

-define(METRIC_CNT_HTTP_SUCCESS_TOTAL, [count, http, success, total]).
-define(METRIC_CNT_HTTP_FAILURE_TOTAL, [count, http, failure, total]).
-define(METRIC_CNT_HTTP_FAILURE(Code), [count, http, failure, Code]).



%% Timings



-define(METRIC_TIM_REQ_TOTAL, [timing, req, total]).
-define(METRIC_TIM_REQ_INFO, [timing, req, info]).
-define(METRIC_TIM_REQ_TANK, [timing, req, tank]).

-define(METRIC_TIM_HTTP_TOTAL, [timing, http, total]).
-define(METRIC_TIM_HTTP_INFO, [timing, http, info]).
-define(METRIC_TIM_HTTP_TANK, [timing, http, tank]).



%% Gauges


