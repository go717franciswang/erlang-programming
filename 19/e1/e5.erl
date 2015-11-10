-module(e5).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

filter([], _N) -> [];
filter([H | T], N) when H =< N -> [H | filter(T, N)];
filter([_H | T], N) -> filter(T, N).

reverse(L) -> reverse(L, []).
reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

concatenate([]) -> [];
concatenate([H | T]) -> H ++ concatenate(T).

flatten(N) when is_integer(N) -> N;
flatten([]) -> [];
flatten([H | T]) -> 
    if 
        is_list(H) -> concatenate([flatten(H), flatten(T)]);
        true -> [H | flatten(T)]
    end.


