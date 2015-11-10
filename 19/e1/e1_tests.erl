-module(e1_tests).
-include_lib("eunit/include/eunit.hrl").

sum_test() ->
    ?assertEqual(15, e1:sum(5)),
    ?assertEqual(6, e1:sum(1,3)),
    ?assertEqual(6, e1:sum(6,6)).

