%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, code_upgrade/1]).
-vsn(1.3).

new() -> gb_trees:empty().

write(Key, Data, Db) -> gb_trees:insert(Key, Data, Db).

read(Key, Db) ->
  case gb_trees:lookup(Key, Db) of
    none         -> {error, instance};
    {value, Data} -> {ok, Data}
  end.

destroy(_Db) -> ok.

delete(Key, Db) -> gb_trees:delete(Key, Db).

code_upgrade(RecordList) -> code_upgrade(RecordList, gb_trees:empty()).
code_upgrade([], GbTree) -> GbTree;
code_upgrade([{K,V}|Rest], GbTree) ->
    NewGbTree = gb_trees:insert(K, V, GbTree),
    code_upgrade(Rest, NewGbTree).
