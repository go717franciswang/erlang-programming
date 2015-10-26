-module(echo).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
    Pid = spawn(echo, loop, []),
    register(echo, Pid).

print(Term) ->
    echo ! {print, Term}.

stop() ->
    echo ! stop.

loop() ->
    receive 
        {print, Msg} ->
            io:format("~p~n", [Msg]),
            loop();
        stop -> true
    end.
