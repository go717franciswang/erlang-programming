-module(ring).
-export([start/3, start_proc/2]).

start(M, N, Message) ->
    ChildPid = spawn(ring, start_proc, [N-1, self()]),
    io:format("~w spawn child process: ~w~n", [self(), ChildPid]),
    receive
        ring_complete ->
            io:format("ring completed~n", []),
            send(ChildPid, M, Message, N-1),
            ChildPid ! stop
    end.

send(_Pid, 0, _Message, _TTL) -> true;
send(Pid, M, Message, TTL) -> 
    Pid ! {message, Message, TTL},
    send(Pid, M-1, Message, TTL).

start_proc(0, Pid) -> 
    Pid ! ring_complete,
    loop(Pid);
start_proc(N, Pid) ->
    ChildPid = spawn(ring, start_proc, [N-1, Pid]),
    io:format("~w spawn child process: ~w~n", [self(), ChildPid]),
    loop(ChildPid).
    
loop(ChildPid) ->
    receive
        {message, Message, TTL} -> 
            io:format("~w got message: ~p~n", [self(), Message]),
            if 
                TTL > 0 -> ChildPid ! {message, Message, TTL-1};
                true -> true
            end,
            loop(ChildPid);
        stop -> 
            ChildPid ! stop,
            io:format("stopping ~w~n", [self()]),
            true
    end.

