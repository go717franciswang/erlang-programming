-module(e6).
-export([quicksort/1, mergesort/1]).

separate(L, N) -> separate(L, N, [], [], []).
separate([], _N, L, M, R) -> { L, M, R };
separate([H | T], N, L, M, R) ->
    if 
        H < N -> separate(T, N, [H|L], M, R);
        H == N -> separate(T, N, L, [H|M], R);
        true -> separate(T, N, L, M, [H|R])
    end.

quicksort([]) -> [];
quicksort(List) ->
    {L, M, R} = separate(List, hd(List)),
    quicksort(L) ++ M ++ quicksort(R).

merge([], R) -> R;
merge(L, []) -> L;
merge([H1|T1], [H2|T2]) ->
    if 
        H1 =< H2 -> [H1 | merge(T1, [H2|T2])];
        true -> [H2 | merge([H1|T1], T2)]
    end.

mergesort(L) when length(L) < 2 -> L;
mergesort([X, Y]) when X =< Y -> [X, Y];
mergesort([X, Y]) when X > Y -> [Y, X];
mergesort(List) ->
    {L, R} = lists:split(trunc(length(List)/2), List),
    merge(mergesort(L), mergesort(R)).

