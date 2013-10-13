-module(hydra_env).

-export([get/1, get/2]).



%% Interface


-spec get(list() | atom()) -> term().
get([Elem | List]) ->
    get(List, application:get_env(hydra, Elem), undefined);

get(Elem) ->
    get([], application:get_env(hydra, Elem), undefined).


-spec get(list() | atom(), term()) -> term().
get([Elem | List], Default) ->
    get(List, application:get_env(hydra, Elem), Default);

get(Elem, Default) ->
    get([], application:get_env(hydra, Elem), Default).


-spec get(list() | atom(), term(), term()) -> term().
get(_List, undefined, Default) ->
    Default;

get([Elem | List], {ok, Env}, Default) ->
    get(List, proplists:get_value(Elem, Env), Default);

get([], {ok, Env}, _Default) ->
    Env.