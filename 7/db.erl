-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include("db.hrl").

new() -> [].

destroy(_Db) -> 0.

read(Key, Db) ->
    case lists:keyfind(Key, 2, Db) of
        false -> { error, instance };
        #data{data=V} -> { ok, V }
    end.

write(Key, Element, Db) ->
    lists:keystore(Key, 1, Db, #data{key=Key, data=Element}).

delete(Key, Db) ->
    lists:keydelete(Key, 2, Db).

match(Element, Db) ->
    case lists:keytake(Element, 3, Db) of
        {value, #data{key=K}, Db2} -> [K | match(Element, Db2)];
        _ -> []
    end.
