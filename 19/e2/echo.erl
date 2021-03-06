%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(echo).
-export([go/0, loop/0]).
-include_lib("eunit/include/eunit.hrl").

go() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msg} ->
            io:format("~w~n",[Msg])
    end,
    Pid ! stop.


loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.

echo_test() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        Reply -> ?assertMatch({_Pid, hello}, Reply)
    end,
    Pid ! stop.

