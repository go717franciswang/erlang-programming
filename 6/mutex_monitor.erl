-module(mutex_monitor).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(mutex_monitor, init, [])).

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
            Reference = monitor(process, Pid),
            Pid ! ok,
            busy(Pid, Reference);
        stop -> 
            terminate()
    end.

busy(Pid, Reference) ->
    receive
        {signal, Pid} ->
            demonitor(Reference),
            free();
        {'DOWN', Reference, process, Pid, _Reason} ->
            demonitor(Reference),
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after 0 -> ok
    end.


