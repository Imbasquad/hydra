-module(hydra_app).

-behaviour(application).

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    ok = unicorn:load(<<"/Users/shizz/code/_etc/squadder/hydra/config.json">>, fun loader/1, fun validator/1),
    hydra_sup:start_link().

stop(_State) ->
    ok.



%% Internals



loader(Contents) ->
    try
        {ok, jiffy:decode(Contents)}
    catch
        _Type:Error ->
            {error, Error}
    end.



validator(Document) ->
    Map = {hash, [
        {<<"rps">>, required, {integer}}
    ]},
    case jiffy_v:validate(Map, Document, fun validator/3) of
        {[], Result} ->
            {ok, Result};
        {Errors, _Result} ->
            {error, Errors}
    end.



validator(validate, [<<"rps">>], Value) when Value =< 0 ->
    {error, <<"RPS value is invalid">>};
validator(validate, _, _) ->
    {ok, valid};
validator(fix, _, _) ->
    {error, invalid}.