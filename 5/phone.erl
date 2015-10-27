-module(phone).
-export([init/0, send_event/1]).
-export([idle/0]).

init() ->
    event_manager:start(billing, [{billing_handler, []}]),
    register(phone, spawn(phone, idle, [])).

send_event(Event) -> phone ! Event.

idle() ->
    flush(),
    io:format("Phone is idle~n", []),
    receive
        {Number, incoming} ->
            start_ringing(),
            ringing(Number);
        off_hook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    flush(),
    receive
        {Number, other_on_hook} ->
            stop_ringing(),
            idle();
        {Number, off_hook} ->
            stop_ringing(),
            connected(Number)
    end.

dial() ->
    flush(),
    receive
        on_hook -> idle();
        {Number, off_hook} -> connected(Number)
    end.

connected(Number) ->
    flush(),
    io:format("Connected to ~w~n", [Number]),
    event_manager:send_event(billing, {connect, Number}),
    receive
        on_hook -> 
            event_manager:send_event(billing, {disconnect, Number}),
            idle()
    end.

start_ringing() -> io:format("Ringing..~n", []).
start_tone() -> io:format("Start Tune..~n", []).
stop_ringing() -> io:format("Ringing Stopped~n", []).
flush() ->
    receive _ -> flush()
    after 0 -> true
    end.
