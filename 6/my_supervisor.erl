%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(my_supervisor).
-export([start_link/2, stop/1, info/1, start_child/2, stop_child/2]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A, Type} | ChildSpecList]) ->
    case (catch apply(M,F,A)) of
        {ok, Pid} ->
            % use now() as unique id to identify a child
            [{Pid, {M,F,A,Type,[]}, now()}|start_children(ChildSpecList)];
        _ ->
            start_children(ChildSpecList)
    end.

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages. 
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated 
%% child, replacing its entry in the list of children stored in the ChildList variable:

restart_child(Pid, ChildList) ->
    {value, {Pid, {M,F,A,Type,RestartTs}, Id}} = lists:keysearch(Pid, 1, ChildList),
    NewChildList = lists:keydelete(Pid,1,ChildList),
    case Type of
         transient -> NewChildList;
         permanent -> 
            NewRestartTs = keep_within_60_seconds(RestartTs),
            if 
                length(NewRestartTs) >= 5 ->
                    io:format("Stopped restarting ~w:~w after 5 restarts in 1 minute", [M, F]),
                    NewChildList;
                true ->
                    {ok, NewPid} = apply(M,F,A),
                    [{NewPid, {M,F,A,Type,[ts()|NewRestartTs]}, Id}|NewChildList]
            end
    end.

ts() ->
    calendar:datetime_to_gregorian_seconds({date(), time()}).

keep_within_60_seconds(RestartTs) ->
    [X || X <- RestartTs, X > ts()-60].

loop(ChildList) ->
    receive
        {'EXIT', Pid, _Reason} ->
            NewChildList = restart_child(Pid, ChildList),
            loop(NewChildList);
        {stop, From}  ->
            From ! {reply, terminate(ChildList)};
        {info, From} ->
            From ! {reply, ChildList},
            loop(ChildList);
        {start_child, From, ChildSpec} ->
            case start_child(ChildSpec) of
                {Pid, Spec, Id} ->
                    NewChildList = [{Pid, Spec, Id}|ChildList],
                    From ! {reply, Id},
                    loop(NewChildList);
                {error, cannot_start_child} ->
                    From ! {reply, error},
                    loop(ChildList)
            end;
        {stop_child, From, Id} ->
            case lists:keysearch(Id, 3, ChildList) of
                {value, {Pid, _Spec, _Id}} -> 
                    unlink(Pid),
                    exit(Pid, kill),
                    From ! {reply, ok},
                    loop(lists:keydelete(Id, 3, ChildList));
                false -> 
                    From ! {reply, error},
                    loop(ChildList)
            end
    end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the 
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated 
%% the stop call:

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

info(Name) ->
    Name ! {info, self()},
    receive {reply, Reply} -> Reply end.

start_child(Name, ChildSpec) ->
    Name ! {start_child, self(), ChildSpec},
    receive {reply, Reply} -> Reply end.

start_child({M,F,A,Type}) ->
    case (catch apply(M,F,A)) of
        {ok, Pid} -> {Pid, {M,F,A,Type,[]}, now()};
        _ -> {error, cannot_start_child}
    end.

stop_child(Name, Id) ->
    Name ! {stop_child, self(), Id},
    receive {reply, Reply} -> Reply end.

