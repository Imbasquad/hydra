-module(hydra_env).

-export([get/1, get/2]).



-type env_key() :: atom().
-type env_path() :: [env_key()].
-type env_success_ret() :: term().
-type env_failure_ret() :: undefined.
-type env_ret() :: env_success_ret() | env_failure_ret().



%% Interface



-spec get(Key :: env_key() | env_path()) ->
    env_ret().
get([Key | Path]) ->
    get(Path, application:get_env(hydra, Key), undefined);

get(Key) ->
    get([], application:get_env(hydra, Key), undefined).


-spec get(Path :: env_path(), Default :: term()) ->
    env_ret().
get([Key | Path], Default) ->
    get(Path, application:get_env(hydra, Key), Default);

get(Key, Default) ->
    get([], application:get_env(hydra, Key), Default).


-spec get(Path :: env_path(), Part :: term(), Default :: term()) ->
    env_ret().
get(_Path, undefined, Default) ->
    Default;

get([Key | Path], {ok, Env}, Default) ->
    get(Path, proplists:get_value(Key, Env), Default);

get([], {ok, Env}, _Default) ->
    Env.