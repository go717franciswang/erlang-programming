-module(e2).
-export([create/1, reverse_create/1]).

create(1) -> [1];
create(N) when N > 1 -> create(N-1)++[N].

reverse_create(1)-> [1];
reverse_create(N) when N > 1 -> [N | reverse_create(N-1)].
