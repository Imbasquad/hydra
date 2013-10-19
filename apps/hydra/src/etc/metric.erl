-module(metric).

-export([inc/1, inc/2, dec/1, dec/2, gauge/2, timewrap/2, key/1]).



%% Interface



inc(Keys) ->
    inc(Keys, 1).

inc(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:increment(key(Key), Value)
    end, Keys).



dec(Keys) ->
    dec(Keys, 1).

dec(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:decrement(key(Key), Value)
    end, Keys).



gauge(Keys, Value) ->
    lists:foreach(fun(Key) ->
        estatsd:gauge(key(Key), Value)
    end, Keys).



timewrap(Keys, Fun) ->
    T = erlang:now(),
    Result = Fun(),
    lists:foreach(fun(Key) ->
        estatsd:timing(key(Key), T)
    end, Keys),
    Result.



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



to_binary(Elem) when is_atom(Elem) ->
    atom_to_binary(Elem, latin1);

to_binary(Elem) when is_integer(Elem) ->
    list_to_binary(integer_to_list(Elem));

to_binary(Elem) when is_list(Elem) ->
    list_to_binary(Elem);

to_binary(Elem) when is_binary(Elem) ->
    Elem.

