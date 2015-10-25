-module(e9).
-export([file_to_raw_doc/1, page_to_words/1, raw_doc_to_index/1, update_index/3, summarize_running_seq/1, print_index/1]).

chunk([]) -> [];
chunk(L) ->
    LinesPerPage = 30,
    if 
        length(L) > LinesPerPage -> 
            {H, T} = lists:split(LinesPerPage, L),
            [H | chunk(T)];
        true -> L
    end.

file_to_raw_doc(Filename) ->
    {ok, Fp} = file:open(Filename, [read]),
    Lines = fp_to_lines(Fp),
    chunk(Lines).

fp_to_lines(FilePointer) ->
    case file:read_line(FilePointer) of
        {ok, Line} -> [Line | fp_to_lines(FilePointer)];
        eof -> []
    end.

page_to_words(Lines) ->
    Page = lists:concat(Lines),
    string:tokens(Page, " ,\r\n.()!\"';:").

raw_doc_to_index(RawDoc) -> raw_doc_to_index(RawDoc, 1, []).
raw_doc_to_index([], _CurrentPageNum, Index) -> Index;
raw_doc_to_index([Lines | RestRawDoc], CurrentPageNum, Index) ->
    Words = page_to_words(Lines),
    raw_doc_to_index(RestRawDoc, CurrentPageNum+1, update_index(Words, Index, CurrentPageNum)).

update_index([], Index, _) -> Index;
update_index([Word | Tail], Index, PageNum) ->
    Record = case lists:keyfind(Word, 1, Index) of
        false -> {Word, [PageNum]};
        {_K,V} -> {Word, V ++ [PageNum]}
    end,
    update_index(Tail, lists:keystore(Word, 1, Index, Record), PageNum).

summarize_running_seq([]) -> [];
summarize_running_seq([H | T]) -> summarize_running_seq(T, H, H).
summarize_running_seq([], Begin, End) -> [{Begin, End}];
summarize_running_seq([H | T], Begin, End) ->
    if 
        H =< End+1 -> summarize_running_seq(T, Begin, H);
        true -> [{Begin, End} | summarize_running_seq(T, H, H)]
    end.

print_record({Word, PageNums}) ->
    Summarized = summarize_running_seq(PageNums),
    Joined = string:join([
            if 
                Begin == End -> integer_to_list(Begin);
                true -> lists:concat([Begin, "-", End])
            end || {Begin,End} <- Summarized], ","),
    io:format("~p\t~p~n", [Word, Joined]).

print_index([]) -> 0;
print_index([Record | RestIndex]) -> print_record(Record), print_index(RestIndex).


