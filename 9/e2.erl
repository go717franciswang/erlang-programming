-module(e2).
-export([divisible_by_three/0, ints_squared/1, intersection/2, symmetric_difference/2]).

divisible_by_three() ->
    [X || X <- lists:seq(1,10), X rem 3 == 0].

ints_squared(Xs) ->
    [X*X || X <- Xs, is_integer(X)].

intersection(L1, L2) ->
    [X || X <- L1, Y <- L2, X == Y].

symmetric_difference(L1, L2) ->
    [X || X <- L1, not(lists:member(X, L2))] ++ [Y || Y <- L2, not(lists:member(Y, L1))].
