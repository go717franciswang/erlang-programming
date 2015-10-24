-module(boo).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(true) -> false;
b_not(_) -> true.

b_and(true, true) -> true;
b_and(_, _) -> false.

b_or(false, false) -> false;
b_or(_, _) -> true.

b_nand(A, B) -> b_not(b_and(A, B)).

