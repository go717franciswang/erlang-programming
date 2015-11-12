-module(e6).
-export([quicksort/1, mergesort/1]).
-include_lib("eunit/include/eunit.hrl").

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

% using list comprehension: http://www.erlang.org/doc/programming_examples/list_comprehensions.html
% sort([Pivot|T]) ->
%     sort([ X || X <- T, X < Pivot]) ++
%     [Pivot] ++
%     sort([ X || X <- T, X >= Pivot]);
% sort([]) -> [].

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

quicksort_test() ->
    ?assertEqual([1,2,3,4], quicksort([2,4,1,3])).

mergesort_test() ->
    ?assertEqual([1,2,3,4], mergesort([2,4,1,3])).
