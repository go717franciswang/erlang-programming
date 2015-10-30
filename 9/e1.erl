-module(e1).
-export([print_n/1, keep_small/2, print_even/1, concat/1, my_sum/1]).

print_n(N) ->
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, lists:seq(1,N)).

keep_small(L, N) ->
    lists:filter(fun(X) -> X =< N end, L).

print_even(N) ->
    Evens = lists:filter(fun(X) -> X rem 2 == 0 end, lists:seq(1,N)),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Evens).

concat(Xss) ->
    lists:foldl(fun(X, L) -> L ++ X end, [], Xss).

my_sum(Xs) ->
    lists:foldl(fun(X, Total) -> Total + X end, 0, Xs).
