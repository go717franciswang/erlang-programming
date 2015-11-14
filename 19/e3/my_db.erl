-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1, code_upgrade/0]).
-export([loop/1]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    register(my_db, spawn(my_db, loop, [db:new()])).

stop() -> call(stop).
write(Key, Element) -> call({write, Key, Element}).
delete(Key) -> call({delete, Key}).
read(Key) -> call({read, Key}).
match(Element) -> call({match, Element}).
code_upgrade() -> call(code_upgrade).

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
        {request, Pid, code_upgrade} ->
            NewDb = db:code_upgrade(Db),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, stop} -> 
            reply(Pid, ok)
    end.

my_test() ->
    eunit:test(
        {spawn,
            {setup,
                fun() ->  % setup
                        start(),
                        write(bob, handyman),
                        write(chris, projectleader)
                end,
                fun(_) -> % cleanup
                        stop(),
                        code:del_path("./patches"),
                        code:soft_purge(db),
                        code:load_file(db)
                end,
                fun() ->
                        ?assertEqual({ok, handyman}, read(bob)),
                        code:add_patha("./patches"),
                        code:soft_purge(db),
                        code:load_file(db),
                        ?assertEqual(ok, code_upgrade()),
                        ?assertEqual({ok, handyman}, read(bob)),
                        ?assertEqual(ok, delete(bob)),
                        ?assertEqual(ok, write(bob, manager))
                end
            }
        }
    ).



