-module(e1).
-export([start/1, stop/0, insert/2, lookup/1]).
-export([init/1]).

start(Nodes) ->
    register(dist, spawn(?MODULE, init, [Nodes])).

init(Nodes) ->
    Pids = lists:map(
        fun(Node) ->
                spawn_link(Node, my_db, loop, [db:new()])
        end, Nodes),
    loop(Pids).

loop(Pids) ->
    receive
        {request, From, stop} ->
            lists:foreach(fun(Pid) -> call_db(stop, Pid) end, Pids),
            reply(From, ok);
        {request, From, Request} -> 
            Reply = request(Request, Pids),
            reply(From, Reply),
            loop(Pids)
    end.

call_db(Message, Pid) ->
    io:format("DBG-- calling db (~p): ~p~n", [Pid, Message]),
    Pid ! {request, self(), Message},
    receive {reply, Reply} -> Reply end.

request({insert, K, V}, Pids) ->
    lists:foreach(fun(Pid) -> call_db({write, K, V}, Pid) end, Pids),
    ok;
request({lookup, K}, Pids) ->
    Pid = lists:nth(random:uniform(length(Pids)), Pids),
    call_db({read, K}, Pid).

stop() ->
    call(stop).

insert(K, V) ->
    call({insert, K, V}).

lookup(K) ->
    call({lookup, K}).

call(Message) ->
    Ref = make_ref(),
    dist ! {request, {Ref, self()}, Message},
    receive {reply, Ref, Reply} -> Reply end.

reply({Ref, To}, Reply) ->
    To ! {reply, Ref, Reply}.
