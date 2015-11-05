-module(my_db).
-export([start_link/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).
-behavior(gen_server).

start_link() ->
    gen_server:start_link({local, my_db}, ?MODULE, [], []).

init(_) ->
    {ok, db:new()}.

stop() -> gen_server:cast(my_db, stop).
write(Key, Element) -> gen_server:call(my_db, {write, Key, Element}).
delete(Key) -> gen_server:call(my_db, {delete, Key}).
read(Key) -> gen_server:call(my_db, {read, Key}).
match(Element) -> gen_server:call(my_db, {match, Element}).

handle_cast(stop, _) ->
    {stop, normal}.

terminate(_Reason, _Db) ->
    ok.

handle_call({write, K, V}, _From, Db) ->
    {reply, ok, db:write(K, V, Db)};
handle_call({delete, K}, _From, Db) ->
    {reply, ok, db:delete(K, Db)};
handle_call({read, K}, _From, Db) ->
    {reply, db:read(K, Db), Db};
handle_call({match, V}, _From, Db) ->
    {reply, db:match(V, Db), Db}.
