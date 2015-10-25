-module(db2).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> 0.

read(Key, Db) ->
    case lists:keyfind(Key, 1, Db) of
        false -> { error, instance };
        {_K,V} -> { ok, V }
    end.

write(Key, Element, Db) ->
    lists:keystore(Key, 1, Db, {Key, Element}).

delete(Key, Db) ->
    lists:keydelete(Key, 1, Db).

match(Element, Db) ->
    case lists:keytake(Element, 2, Db) of
        {value, {K,_V}, Db2} -> [K | match(Element, Db2)];
        _ -> []
    end.
