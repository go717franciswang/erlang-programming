-module(e1).
-export([server/0]).
-export([get_request/3]).

server() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
    wait_connect(ListenSocket, 0).

wait_connect(ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Got connection~n"),

    Pid = spawn(?MODULE, get_request, [Socket, [], Count]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(ListenSocket, Count+1).

get_request(Socket, BinaryList, Count) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Binary} ->
            get_request(Socket, [Binary|BinaryList], Count);
        {error, closed} ->
            handle(lists:reverse(BinaryList), Count);
        {error, timeout} ->
            handle(lists:reverse(BinaryList), Count)
    end.

handle(Binary, Count) ->
    Chunk = lists:map(fun(X) -> binary_to_list(X) end, Binary),
    Payload = string:join(Chunk, ""),
    io:format("~p: ~s~n", [Count, Payload]).

