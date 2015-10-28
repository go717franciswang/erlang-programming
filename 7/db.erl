-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include("db.hrl").

new() -> [].

destroy(_Db) -> 0.

read(Key, Db) ->
    case lists:keyfind(Key, #data.key, Db) of
        false -> { error, instance };
        #data{data=V} -> { ok, V }
    end.

write(Key, Element, Db) ->
    lists:keystore(Key, #data.key, Db, #data{key=Key, data=Element}).

delete(Key, Db) ->
    lists:keydelete(Key, #data.key, Db).

match(Element, Db) ->
    [K || #data{key=K, data=V} <- Db, V == Element].
