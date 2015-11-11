-module(e2).
-export([create/1, reverse_create/1]).
-include_lib("eunit/include/eunit.hrl").

create(1) -> [1];
create(N) when N > 1 -> create(N-1)++[N].

reverse_create(1)-> [1];
reverse_create(N) when N > 1 -> [N | reverse_create(N-1)].

create_test() ->
    ?assertEqual([1,2,3], create(3)).

reverse_create_test() ->
    ?assertEqual([3,2,1], reverse_create(3)).
