-module(stats_handler).
-export([init/1, handle_event/2, terminate/1]).

init(InitStats) -> InitStats.

terminate(Stats) -> Stats.

handle_event({Type, _Id, Description}, Stats) ->
    Record = case lists:keyfind({Type, Description}, 1, Stats) of
                 false -> {{Type, Description}, 1};
                 {K,V} -> {K, V+1}
             end,
    lists:keystore({Type, Description}, 1, Stats, Record);
handle_event(_, Stats) -> Stats.
