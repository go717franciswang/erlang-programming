-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

my_test() ->
    Db = db:new(),
    ?assertEqual([], Db),

    Db1 = db:write(francesco, london, Db),
    ?assertEqual([{francesco,london}], Db1),

    Db2 = db:write(lelle, stockholm, Db1),
    ?assertEqual([{francesco,london},{lelle,stockholm}], Db2),

    ?assertEqual({ok,london}, db:read(francesco, Db2)),

    Db3 = db:write(joern, stockholm, Db2),
    ?assertEqual([{francesco,london},{lelle,stockholm},{joern,stockholm}], Db3),

    ?assertEqual({error,instance}, db:read(ola, Db3)),

    ?assertEqual([lelle,joern], db:match(stockholm, Db3)),

    Db4 = db:delete(lelle, Db3),
    ?assertEqual([{francesco,london},{joern,stockholm}], Db4),

    ?assertEqual([joern], db:match(stockholm, Db4)).

