-module(mutex_link).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, Pid=spawn_link(mutex_link, init, [])),
    {ok, Pid}.

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

init() ->
    free().

free() ->
    receive
        {wait, Pid} ->
            try 
                link(Pid),
                Pid ! ok,
                busy(Pid)
            catch 
                error:noproc -> free()
            end;
        stop -> 
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            unlink(Pid),
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after 0 -> ok
    end.


