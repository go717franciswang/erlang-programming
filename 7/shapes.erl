-module(shapes).
-export([perimeter/1, area/1]).
-include("shapes.hrl").

perimeter(#circle{radius=R}) -> 2*math:pi()*R;
perimeter(#rectangle{length=L,width=W}) -> 2*(L+W);
perimeter(#triangle{a=A,b=B,c=C}) -> A+B+C.

area(#circle{radius=R}) -> math:pi()*R*R;
area(#rectangle{length=L,width=W}) -> L*W;
area(#triangle{a=A,b=B,c=C}) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).
