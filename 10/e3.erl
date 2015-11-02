-module(e3).
-export([start/0, stop/0]).
-export([log_message/1, log_ack/1, get_avg_response_time/0, are_all_message_acked/0]).
-export([init/0]).
-export([test/0]).

start() ->
    register(mylog, spawn(?MODULE, init, [])).

init() ->
    % each row is {MessageId, SentAt, AckAt}
    % ordered by SentAt to ease get_avg_response_time calc
    ets:new(mylog, [ordered_set, named_table, {keypos, 2}]),
    ets:new(idToSentAt, [named_table]),
    loop().

loop() ->
    receive
        {request, From, stop} ->
            reply(From, ok);
        {request, From, Request} ->
            Reply = request(Request),
            reply(From, Reply),
            loop()
    end.

request({log_message, MessageId}) ->
    SentAt = now(),
    ets:insert(mylog, {MessageId, SentAt, undefined}),
    ets:insert(idToSentAt, {MessageId, SentAt});
request({log_ack, MessageId}) ->
    case ets:lookup(idToSentAt, MessageId) of
        [{MessageId, SentAt}] -> 
            ets:insert(mylog, {MessageId, SentAt, now()});
        _ -> error
    end;
request({get_avg_response_time}) ->
    SentAt = ets:last(mylog),
    RecentResponseTimes = get_recent_response_time(SentAt),
    case length(RecentResponseTimes) > 0 of
        true -> lists:sum(RecentResponseTimes) / length(RecentResponseTimes);
        _ -> error
    end;
request({are_all_message_acked}) ->
    ets:foldl(
        fun({_,_,AckAt}, AllAcked) -> 
                AllAcked andalso AckAt /= undefined 
        end, true, mylog).

get_recent_response_time(SentAt) -> 
    {MegaSec, Sec, MicroSec} = now(),
    case {MegaSec, Sec-1, MicroSec} > SentAt of
        true -> [];
        _ ->
            NextSentAt = ets:prev(mylog, SentAt),
            case ets:lookup(mylog, SentAt) of
                [{_, _, undefine}] -> 
                    get_recent_response_time(NextSentAt);
                [{_, SentAt, AckAt}] ->
                    [diff(SentAt, AckAt)|get_recent_response_time(NextSentAt)]
            end
    end.

diff(SentAt, AckAt) ->
    sec(AckAt) - sec(SentAt).

sec({MegaSec,Sec,MicroSec}) ->
    MegaSec*1.0e6 + Sec + MicroSec/1.0e6.

log_message(MessageId) ->
    call({log_message, MessageId}).

log_ack(MessageId) ->
    call({log_ack, MessageId}).

get_avg_response_time() ->
    call({get_avg_response_time}).

are_all_message_acked() ->
    call({are_all_message_acked}).

call(Message) ->
    Ref = make_ref(),
    mylog ! {request, {Ref, self()}, Message},
    receive {reply, Ref, Reply} -> Reply end.

reply({Ref,To}, Reply) ->
    To ! {reply, Ref, Reply}.

stop() ->
    call(stop).

test() ->
    start(),
    MessageIds = lists:seq(1, 1000),
    lists:foreach(fun(MessageId) -> log_message(MessageId) end, MessageIds),
    lists:foreach(fun(MessageId) -> log_ack(MessageId) end, MessageIds),

    io:format("are_all_message_acked: ~p~n", [are_all_message_acked()]),

    io:format("avg_response_time: ~p~n", [get_avg_response_time()]),

    log_message(notYetAcked),
    io:format("are_all_message_acked: ~p~n", [are_all_message_acked()]),

    log_ack(notYetAcked),
    io:format("are_all_message_acked: ~p~n", [are_all_message_acked()]),

    stop().
