-module(e1).
-export([sum/1]).

sum(0) -> 0;
sum(N) -> N+sum(N-1).
