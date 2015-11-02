-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([loop/1]).

start() ->
    register(my_db, spawn(my_db, loop, [db:new()])).

stop() -> call(stop).
write(Key, Element) -> call({write, Key, Element}).
delete(Key) -> call({delete, Key}).
read(Key) -> call({read, Key}).
match(Element) -> call({match, Element}).

call(Message) ->
    my_db ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

reply(To, Message) -> To ! {reply, Message}.

loop(Db) ->
    receive
        {request, Pid, {write, Key, Element}} -> 
            NewDb = db:write(Key, Element, Db),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, {delete, Key}} -> 
            NewDb = db:delete(Key, Db),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, {read, Key}} -> 
            reply(Pid, db:read(Key, Db)),
            loop(Db);
        {request, Pid, {match, Element}} -> 
            reply(Pid, db:match(Element, Db)),
            loop(Db);
        {request, Pid, stop} -> 
            reply(Pid, ok)
    end.
