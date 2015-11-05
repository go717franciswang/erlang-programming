-module(db_sup).
-export([start_link/0]).
-export([init/1]).
-behavior(supervisor).

start_link() ->
    supervisor:start_link({local, db_sup}, ?MODULE, []).

init(_) ->
    Child = {db, {my_db, start_link, []}, 
        permanent, 30000, worker, [my_db, db]},
    {ok, {{one_for_all, 5, 60*60*1000}, [Child]}}.




