-module(show_eval).
-export([test/0]).
-include("show_eval.hrl").

test() -> 
    A = 1,
    B = 1,
    ?SHOW_EVAL(A+B).
