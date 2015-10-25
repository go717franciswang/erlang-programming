#!/usr/bin/env escript

main(_) ->
    Raw = e9:file_to_raw_doc("the_book_of_war.txt"),
    Index = e9:raw_doc_to_index(Raw),
    % io:format("~p~n", [Index]).

    % Summarized = e9:summarize_running_seq([1,1,2,4,5,6,6,98,100,102,102]),
    % io:format("~p~n", [Summarized]).

    e9:print_index(lists:sublist(Index, 50)).

