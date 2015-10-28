#!/usr/bin/env escript
-include("person.hrl").

foobar(P) when not(is_record(P, person)) -> not_person;
foobar(P) when P#person.name == "Joe" -> true.

main(_) ->
    io:format("~p~n", [foobar(#person{name="Joe"})]),
    io:format("~p~n", [foobar({person, "Joe", 0, "", undefined})]),
    io:format("~p~n", [foobar(not_person)]).






