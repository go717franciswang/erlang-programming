%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(index).

-export([index/1,processFile/1,prettyEntry/1,accumulate/1, prettyList/1]). 

index(File) ->
    ets:new(indexTable, [ordered_set, named_table]),
    processFile(File),
    prettyIndex().

processFile(File) ->
    {ok,IoDevice} = file:open(File,[read]),
    processLines(IoDevice,1).

processLines(IoDevice,N) ->
    case io:get_line(IoDevice,"") of
        eof ->
            ok;
        Line ->
            processLine(Line,N),
            processLines(IoDevice,N+1)
    end.

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
    Words = re:split(Line,?Punctuation),
    processWords(Words,N).

processWords(Words,N) ->
    case Words of
        [] -> ok;
        [W|Rest] ->
            Word = binary_to_list(W),
            if
                length(Word) > 3 ->
                    Normalise = string:to_lower(Word),
                    ets:insert(indexTable,{{Normalise , trunc(N/30)}});
                true -> ok
            end,
            processWords(Rest,N)
    end.

prettyIndex() ->
    case ets:first(indexTable) of
        '$end_of_table' ->
            ok;
        First  ->
            case First of
                {Word, N} ->
                    IndexEntry = {Word, [N]}
            end,
            prettyIndexNext(First,IndexEntry)
    end.


prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
    Next = ets:next(indexTable,Entry),
    case Next of
        '$end_of_table' ->
            prettyEntry(IndexEntry);
        {NextWord, M}  ->
            if
                NextWord == Word ->
                    prettyIndexNext(Next,{Word, [M|Lines]});
                true ->
                    prettyEntry(IndexEntry),
                    prettyIndexNext(Next,{NextWord, [M]})
            end
    end.

prettyEntry({Word, Lines}) ->
    io:format("~s~s~n", [pad(20, Word), prettyList(accumulate(Lines))]).

accumulate([N|Rest]) -> accumulate(N, N, Rest, []).
accumulate(Start, End, [], Acc) -> [rangeToTuple(Start,End)|Acc];
accumulate(Start, End, [N|Rest], Acc) ->
    if 
        Start =< N+1 -> accumulate(N, End, Rest, Acc);
        true -> 
            NewAcc = [rangeToTuple(Start, End)|Acc],
            accumulate(N, N, Rest, NewAcc)
    end.

rangeToTuple(Start, End) ->
    case Start of
        End -> {Start};
        _ -> {Start,End}
    end.

prettyList(Ranges) ->
    Strings = lists:map(
        fun({A}) -> 
                integer_to_list(A);
            ({A,B}) -> 
                integer_to_list(A) ++ "-" ++ integer_to_list(B) 
        end, Ranges),
    string:join(Strings, ",").

pad(N, Word) ->
    string:left(Word, N, $\ ).
