-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() -> [].

destroy(_Db) -> 0.

read(_Key, []) -> { error, instance };
read(Key, [{K,V} | T]) ->
    case K of
        Key -> { ok, V };
        _ -> read(Key, T)
    end.

write(Key, Element, []) -> [{Key, Element}];
write(Key, Element, [{K,V} | T]) ->
    case K of
        Key -> [{ Key, Element } | T];
        _ -> [{K,V} | write(Key, Element, T)]
    end.

delete(_Key, []) -> [];
delete(Key, [{K,V} | T]) ->
    case K of
        Key -> T;
        _ -> [{K,V} | delete(Key, T)]
    end.

match(_Element, []) -> [];
match(Element, [{K,V} | T]) ->
    case V of
        Element -> [K | match(Element, T)];
        _ -> match(Element, T)
    end.

