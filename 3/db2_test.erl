#!/usr/bin/env escript

main(_) ->
    Db = db2:new(),
    io:format("~p~n", [Db]),

    Db1 = db2:write(francesco, london, Db),
    io:format("~p~n", [Db1]),

    Db2 = db2:write(lelle, stockholm, Db1),
    io:format("~p~n", [Db2]),

    io:format("~p~n", [db2:read(francesco, Db2)]),

    Db3 = db2:write(joern, stockholm, Db2),
    io:format("~p~n", [Db3]),

    io:format("~p~n", [db2:read(ola, Db3)]),

    io:format("~p~n", [db2:match(stockholm, Db3)]),

    Db4 = db2:delete(lelle, Db3),
    io:format("~p~n", [Db4]),

    io:format("~p~n", [db2:match(stockholm, Db4)]).



