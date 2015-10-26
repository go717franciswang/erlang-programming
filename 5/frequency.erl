-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].

% client functions
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

% server
loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} -> 
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            case length(element(2, Frequencies)) of
                0 -> reply(Pid, ok);
                _ -> 
                    reply(Pid, {error, existing_allocated_freq}),
                    loop(Frequencies)
            end
    end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case allocated_count(Allocated, Pid) of
        Count when Count < 3 -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        _ -> {{[Freq|Free], Allocated}, {error, cannot_allocate_more_than_three}}
    end.

allocated_count(Allocated, Pid) -> allocated_count(Allocated, Pid, 0).
allocated_count([], _Pid, Acc) -> Acc;
allocated_count([{_Freq, Pid0}|Allocated], Pid, Acc) ->
    case Pid0 of
        Pid -> allocated_count(Allocated, Pid, Acc+1);
        _ -> allocated_count(Allocated, Pid, Acc)
    end.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keyfind(Freq, 1, Allocated) of
        false -> {{Free, Allocated}, {error, key_not_allocated}};
        {Freq, Pid} ->
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {{[Freq|Free], NewAllocated}, ok};
        {Freq, _Pid} -> {{Free, Allocated}, {error, freq_does_not_belong_to_pid}}
    end.
