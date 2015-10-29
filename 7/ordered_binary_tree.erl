-module(ordered_binary_tree).
-export([b_sum/1, b_max/1, is_ordered/1, insert/2]).
-include("ordered_binary_tree.hrl").

b_sum(undefined) -> 0;
b_sum(#node{left=L,right=R,value=V}) -> V+b_sum(L)+b_sum(R).

b_max(#node{left=undefined,right=undefined,value=V}) -> V;
b_max(#node{left=undefined,right=R,value=V}) -> max(b_max(R), V);
b_max(#node{left=L,right=undefined,value=V}) -> max(b_max(L), V);
b_max(#node{left=L,right=R,value=V}) -> max(max(b_max(L), b_max(R)), V).

is_ordered(undefined) -> true;

is_ordered(#node{left=undefined,right=undefined}) -> true;
is_ordered(#node{left=undefined,right=R,value=V}) -> R#node.value >= V andalso is_ordered(R);
is_ordered(#node{left=L,right=undefined,value=V}) -> L#node.value =< V andalso is_ordered(L);
is_ordered(#node{left=L,right=R,value=V}) -> 
    L#node.value =< V andalso R#node.value >= V andalso is_ordered(L) andalso is_ordered(R).

insert(V, undefined) -> #node{value=V};
insert(V, #node{left=L,value=V0}=N) when V0 >= V -> N#node{left=insert(V, L)};
insert(V, #node{right=R}=N) -> N#node{right=insert(V, R)}.
