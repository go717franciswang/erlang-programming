#!/usr/bin/env escript

-include("ordered_binary_tree.hrl").

main(_) ->
    R = #node{value=3},
    R1 = ordered_binary_tree:insert(2, R),
    R2 = ordered_binary_tree:insert(4, R1),
    R3 = ordered_binary_tree:insert(5, R2),
    R4 = ordered_binary_tree:insert(3, R3),
    io:format("~p~n", [R4]),

    io:format("sum: ~p~n", [ordered_binary_tree:b_sum(R4)]),
    io:format("max: ~p~n", [ordered_binary_tree:b_max(R4)]),
    io:format("is_ordered: ~p~n", [ordered_binary_tree:is_ordered(R4)]).

