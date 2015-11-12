-module(e5).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).
-include_lib("eunit/include/eunit.hrl").

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

filter_test() ->
    ?assertEqual([1,2,3], filter([1,2,3,4,5], 3)).

reverse_test() ->
    ?assertEqual([3,2,1], reverse([1,2,3])).

concatenate_test() ->
    ?assertEqual([1,2,3,4,five], concatenate([[1,2,3], [], [4,five]])).

flatten_test() ->
    ?assertEqual([1,2,3,4,5,6], flatten([[1,[2,[3],[]]], [[[4]]], [5,6]])).
