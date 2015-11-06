-module(e2).
-export([raise_salary/0]).
-include("muppet.hrl").

raise_salary() ->
    mnesia:transaction(fun() ->
                               mnesia:foldl(fun(#muppet{salary=Salary}=Muppet, _) ->
                                                    mnesia:write(Muppet#muppet{salary=Salary*1.1})
                                            end, ok, muppet)
                       end).

