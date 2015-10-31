-module(e3).
-export([zip/2, zipWith/3]).

zip([], _) -> [];
zip(_, []) -> [];
zip([A|L1], [B|L2]) -> [{A,B}|zip(L1,L2)].

zipWith(F,L1,L2) ->
    [F(A,B) || {A,B} <- zip(L1,L2)].
