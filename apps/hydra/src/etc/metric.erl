-module(metric).

-export([inc/1, inc/2, dec/1, dec/2, gauge/2, timewrap/2, key/1]).



-type metric_path_part() :: atom() | integer() | string() | binary().
-type metric_path() :: [metric_path_part()].
-type metric_key() :: binary().

-export_type([metric_path_part/0, metric_path/0, metric_key/0]).


%% Interface



-spec inc(Keys :: [metric_path()]) ->
    ok.
inc(Keys) ->
    inc(Keys, 1).



-spec inc(Keys :: [metric_path()], Value :: integer()) ->
    ok.
inc(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:increment(key(Key), Value)
    end, Keys),
    ok.



-spec dec(Keys :: [metric_path()]) ->
    ok.
dec(Keys) ->
    dec(Keys, 1).



-spec dec(Keys :: [metric_path()], Value :: integer()) ->
    ok.
dec(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:decrement(key(Key), Value)
    end, Keys),
    ok.



-spec gauge(Keys :: [metric_path()], Value :: integer()) ->
    ok.
gauge(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:gauge(key(Key), Value)
    end, Keys),
    ok.



-spec timewrap(Keys :: [metric_path()], Fun :: fun(() -> term())) ->
    term().
timewrap(Keys, Fun) ->
    T = erlang:now(),
    Result = Fun(),
    lists:foreach(fun(Key) ->
        estatsd:timing(key(Key), T)
    end, Keys),
    Result.



-spec key(Key :: metric_path_part() | metric_path()) ->
    metric_key().
key(Key) when is_binary(Key) ->
    Key;

key(List) when is_list(List) ->
    App = case application:get_application() of
        {ok, Application} -> Application;
        undefined -> console
    end,
    key([App | List], <<>>).

key([], Acc) ->
    Acc;

key([Elem | List], <<>>) ->
    key(List, to_binary(Elem));

key([Elem | List], Acc) ->
    key(List, <<Acc/binary, $., (to_binary(Elem))/binary>>).



%% Internals



-spec to_binary(metric_path_part()) ->
    binary().
to_binary(Elem) when is_atom(Elem) ->
    atom_to_binary(Elem, latin1);

to_binary(Elem) when is_integer(Elem) ->
    list_to_binary(integer_to_list(Elem));

to_binary(Elem) when is_list(Elem) ->
    list_to_binary(Elem);

to_binary(Elem) when is_binary(Elem) ->
    Elem.

