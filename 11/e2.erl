-module(e2).
-export([start/1, stop/0]).
-export([log_insert/2, log_lookup/2, summary/0]).
-export([loop/1]).

% for sake of simplicity
% we will only keep the 10 most recent
% data points

start(Pids) -> 
    register(my_monitor, spawn(?MODULE, loop, [init_data(Pids)])).

init_data(Pids) ->
    lists:map(fun(Pid) -> {Pid, [], []} end, Pids).

loop(Data) ->
    receive
        {request, From, stop} ->
            reply(From, ok);
        {request, From, Request} ->
            {Reply, NewData} = request(Request, Data),
            reply(From, Reply),
            loop(NewData)
    end.

request({log_insert, Pid, Timestamp}, Data) ->
    Tuple = case lists:keyfind(Pid, 1, Data) of
                {Pid, Inserts, Lookups} ->
                    {Pid, [Timestamp|lists:sublist(Inserts, 9)], Lookups}
            end,
    {ok, lists:keystore(Pid, 1, Data, Tuple)};
request({log_lookup, Pid, Timestamp}, Data) ->
    Tuple = case lists:keyfind(Pid, 1, Data) of
                {Pid, Inserts, Lookups} ->
                    {Pid, Inserts, [Timestamp|lists:sublist(Lookups, 9)]}
            end,
    {ok, lists:keystore(Pid, 1, Data, Tuple)};
request(summary, Data) ->
    {lists:map(fun({Pid, Inserts, Lookups}) ->
                 {Pid, {insert_per_sec, calc_rate(Inserts)}, {lookups_per_sec, calc_rate(Lookups)}}
         end, Data), Data}.

calc_rate(Timestamps) when length(Timestamps) > 1 ->
    (length(Timestamps) - 1) / (sec(hd(Timestamps)) - sec(lists:last(Timestamps)));
calc_rate(_) -> undefined.

sec({MegaSec,Sec,MicroSec}) ->
    MegaSec*1.0e6 + Sec + MicroSec/1.0e6.

log_insert(Pid, Timestamps) -> call({log_insert, Pid, Timestamps}).
log_lookup(Pid, Timestamps) -> call({log_lookup, Pid, Timestamps}).
summary() -> call(summary).
stop() -> call(stop).

call(Message) ->
    Ref = make_ref,
    my_monitor ! {request, {Ref, self()}, Message},
    receive {reply, Ref, Reply} -> Reply end.

reply({Ref, To}, Reply) ->
    To ! {reply, Ref, Reply}.

