-module(e3).
-export([print_ints/1, print_even/1]).

print_ints(N) when N > 0 -> 
    print_ints(N-1), 
    io:format("Number:~p~n", [N]);
print_ints(_) -> 0.

print_even(N) when N > 1, N rem 2 == 0 ->
    print_even(N-2),
    io:format("Number:~p~n", [N]);
print_even(N) when N > 1, N rem 2 == 1 ->
    print_even(N-1);
print_even(_) -> 0.
